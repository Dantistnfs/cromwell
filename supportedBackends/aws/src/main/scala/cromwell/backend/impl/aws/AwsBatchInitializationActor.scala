/*
 * Copyright 2018 Amazon.com, Inc. or its affiliates.
 *
 *  Redistribution and use in source and binary forms, with or without
 *  modification, are permitted provided that the following conditions are met:
 *
 *  1. Redistributions of source code must retain the above copyright notice,
 *  this list of conditions and the following disclaimer.
 *
 *  2. Redistributions in binary form must reproduce the above copyright
 *  notice, this list of conditions and the following disclaimer in the
 *  documentation and/or other materials provided with the distribution.
 *
 *  3. Neither the name of the copyright holder nor the names of its
 *  contributors may be used to endorse or promote products derived from
 *  this software without specific prior written permission.
 *
 *  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 *  "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING,
 *  BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND
 *  FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL
 *  THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT,
 *  INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 *  (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 *  SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 *  HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
 *  STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING
 *  IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 *  POSSIBILITY OF SUCH DAMAGE.
 */

package cromwell.backend.impl.aws

import java.io.IOException

import akka.actor.ActorRef
import software.amazon.awssdk.auth.credentials.AwsCredentialsProvider
import software.amazon.awssdk.services.dynamodb.DynamoDbClient
import software.amazon.awssdk.services.dynamodb.model.{AttributeValue, BatchGetItemRequest, KeysAndAttributes}
import software.amazon.awssdk.services.secretsmanager.SecretsManagerClient
import software.amazon.awssdk.services.secretsmanager.model.{CreateSecretRequest, SecretsManagerException, SecretListEntry, UpdateSecretRequest}
import cromwell.filesystems.s3.batch.S3BatchCommandBuilder
import cromwell.backend.standard.{StandardInitializationActor, StandardInitializationActorParams, StandardValidatedRuntimeAttributesBuilder}
import cromwell.backend.{BackendConfigurationDescriptor, BackendWorkflowDescriptor, BackendInitializationData}
import cromwell.core.io.DefaultIoCommandBuilder
import cromwell.core.io.AsyncIoActorClient
import cromwell.core.path.Path
import wom.graph.CommandCallNode
import org.apache.commons.codec.binary.Base64
import spray.json.{JsObject, JsString}
import org.slf4j.{Logger, LoggerFactory}
import cromwell.backend.impl.aws.io.AwsBatchWorkflowPaths

import scala.concurrent.{Future, blocking}
import scala.jdk.CollectionConverters._

case class AwsBatchInitializationActorParams
(
  workflowDescriptor: BackendWorkflowDescriptor,
  ioActor: ActorRef,
  calls: Set[CommandCallNode],
  configuration: AwsBatchConfiguration,
  serviceRegistryActor: ActorRef,
  restarting: Boolean
) extends StandardInitializationActorParams {
  override val configurationDescriptor: BackendConfigurationDescriptor = configuration.configurationDescriptor
}

object AwsBatchInitializationActor {
  private case class AuthFileAlreadyExistsException(path: Path) extends IOException(s"Failed to upload authentication file at $path:" +
    s" there was already a file at the same location and this workflow was not being restarted.")
}

class AwsBatchInitializationActor(params: AwsBatchInitializationActorParams)
  extends StandardInitializationActor(params) with AsyncIoActorClient {

  val Log: Logger = LoggerFactory.getLogger(AwsBatchInitializationActor.getClass)

  override lazy val ioActor = params.ioActor
  private val configuration = params.configuration
  override implicit val system = context.system

  override def beforeAll(): Future[Option[BackendInitializationData]] = {
    configuration.fileSystem match {
      case AWSBatchStorageSystems.s3  => super.beforeAll()
      case _ => {
        initializationData map { data =>
          publishWorkflowRoot(data.workflowPaths.workflowRoot.pathAsString)
          Option(data)
        }
      }
    }
  }

  override lazy val runtimeAttributesBuilder: StandardValidatedRuntimeAttributesBuilder =
    AwsBatchRuntimeAttributes.runtimeAttributesBuilder(configuration)

  private lazy val provider: Future[AwsCredentialsProvider] =
    Future { configuration.awsAuth.provider() }

  lazy val secretsClient: SecretsManagerClient = {
    val builder = SecretsManagerClient.builder()
    configureClient(builder, Option(configuration.awsAuth), configuration.awsConfig.region)
  }

  private def storePrivateDockerToken(token: String) = {
    try {

      val secretName: String = "cromwell/credentials/dockerhub"

      // Check if secret already exists
      // If exists, update it otherwise create it
      val secretsList: List[SecretListEntry] = secretsClient.listSecrets().secretList().asScala.toList

      if(secretsList.exists(_.name == secretName)){
        val secretRequest: UpdateSecretRequest = UpdateSecretRequest.builder()
          .secretId(secretName)
          .secretString(token)
          .build();

        secretsClient.updateSecret(secretRequest);

        Log.info(s"Secret '$secretName' was updated.")
      } else {
        val secretRequest: CreateSecretRequest = CreateSecretRequest.builder()
          .name(secretName)
          .secretString(token)
          .build()

        secretsClient.createSecret(secretRequest)

        Log.info(s"Secret '$secretName' was created.")
      }
    }
    catch {
      case e: SecretsManagerException => Log.warn(e.awsErrorDetails().errorMessage())
    }
  }

  val privateDockerUnencryptedToken: Option[String] = configuration.dockerToken flatMap { dockerToken =>
    new String(Base64.decodeBase64(dockerToken)).split(':') match {
      case Array(username, password) =>
        // unencrypted tokens are base64-encoded username:password
        Option(JsObject(
          Map(
            "username" -> JsString(username),
            "password" -> JsString(password)
          )).compactPrint)
      case _ => throw new RuntimeException(s"provided dockerhub token '$dockerToken' is not a base64-encoded username:password")
    }
  }

  privateDockerUnencryptedToken match {
    case Some(token) => storePrivateDockerToken(token)
    case None => Log.debug("No docker token was passed")
  }

  private def fetchPreemptibilityRecommendations(
    tableName: String,
    calls: Set[CommandCallNode]
  ): Future[Map[String, String]] = {
    if (calls.isEmpty) return Future.successful(Map.empty)
    Future {
      blocking {
        val client = configureClient(
          DynamoDbClient.builder(),
          Option(configuration.awsAuth),
          configuration.awsConfig.region
        )
        try {
          val workflowName = workflowDescriptor.callable.name
          // Deduplicate by FQN: two calls with the same FQN produce the same DynamoDB key,
          // and BatchGetItem rejects duplicate primary keys with ValidationException.
          val allKeys = calls.toList.map(_.identifier.fullyQualifiedName.value).distinct.map { taskFqn =>
            val taskKey = s"$workflowName#$taskFqn"
            Map("task_key" -> AttributeValue.builder().s(taskKey).build()).asJava
          }

          // Retry unprocessed keys (DynamoDB returns them when read capacity is exceeded).
          def fetchChunk(keys: java.util.List[java.util.Map[String, AttributeValue]]): List[(String, String)] = {
            val buf = scala.collection.mutable.ListBuffer.empty[(String, String)]
            var pending: java.util.List[java.util.Map[String, AttributeValue]] = keys
            var attempt = 0
            while (!pending.isEmpty && attempt < 3) {
              attempt += 1
              val resp = client.batchGetItem(
                BatchGetItemRequest.builder()
                  .requestItems(Map(tableName -> KeysAndAttributes.builder().keys(pending).build()).asJava)
                  .build()
              )
              buf ++= resp.responses().asScala.get(tableName).toList.flatMap(_.asScala).flatMap { item =>
                for {
                  keyAttr <- Option(item.get("task_key"))
                  recAttr <- Option(item.get("recommendation"))
                  taskName = keyAttr.s().split('#').last
                  rec      = recAttr.s()
                } yield taskName -> rec
              }
              pending = resp.unprocessedKeys().asScala
                .get(tableName)
                .fold(java.util.Collections.emptyList[java.util.Map[String, AttributeValue]]())(_.keys())
              if (!pending.isEmpty && attempt < 3)
                Log.warn(s"DynamoDB returned ${pending.size()} unprocessed preemptibility keys; retrying (attempt ${attempt + 1}/3)")
            }
            if (!pending.isEmpty)
              Log.warn(s"${pending.size()} preemptibility keys remain unprocessed after 3 attempts; those tasks will use WDL routing")
            buf.toList
          }

          // BatchGetItem is limited to 100 keys per request.
          // Each chunk is isolated: a failure in one chunk does not discard results from others.
          allKeys.grouped(100).flatMap { chunk =>
            try fetchChunk(chunk.asJava)
            catch { case ex: Exception =>
              Log.warn(s"Failed to fetch preemptibility chunk (${chunk.size} tasks): ${ex.getMessage}; skipping chunk")
              Nil
            }
          }.toMap
        } finally {
          client.close()
        }
      }
    }
  }

  override lazy val workflowPaths: Future[AwsBatchWorkflowPaths] = for {
    prov <- provider
  } yield new AwsBatchWorkflowPaths(workflowDescriptor, prov, configuration)

  override lazy val initializationData: Future[AwsBatchBackendInitializationData] = for {
    workflowPaths   <- workflowPaths
    prov            <- provider
    recommendations <- configuration.preemptibilityTableName match {
      case Some(table) =>
        fetchPreemptibilityRecommendations(table, params.calls).recover { case ex =>
          Log.warn(s"Failed to fetch preemptibility recommendations from DynamoDB; routing will use WDL preemptible attribute only. Cause: ${ex.getMessage}")
          Map.empty[String, String]
        }
      case None => Future.successful(Map.empty[String, String])
    }
  } yield AwsBatchBackendInitializationData(workflowPaths, runtimeAttributesBuilder, configuration, prov, recommendations)

  override lazy val ioCommandBuilder =  {
    val conf = Option(configuration) match {
      case Some(cf) => cf
      case None =>  new  AwsBatchConfiguration(params.configurationDescriptor)
    }
    conf.fileSystem match {
      case  AWSBatchStorageSystems.s3 =>  S3BatchCommandBuilder
      case _ =>   DefaultIoCommandBuilder
    }
  }
}
