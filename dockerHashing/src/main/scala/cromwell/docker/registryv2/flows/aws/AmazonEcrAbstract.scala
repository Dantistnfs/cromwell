package cromwell.docker.registryv2.flows.aws

import cats.data.NonEmptyList
import cats.effect.IO
import cromwell.docker.{DockerHashResult, DockerImageIdentifier, DockerInfoActor, DockerRegistryConfig}
import cromwell.docker.registryv2.DockerRegistryV2Abstract
import cromwell.docker.registryv2.DockerRegistryV2Abstract.AcceptAllManifestsHeader
import cromwell.docker.registryv2.flows.aws.EcrUtils.{EcrForbidden, EcrNotFound, EcrUnauthorized}
import org.apache.commons.codec.digest.DigestUtils
import org.http4s.headers.Accept
import org.http4s.{Header, Response, Status}
import org.slf4j.{Logger, LoggerFactory}

abstract class AmazonEcrAbstract(override val config: DockerRegistryConfig) extends DockerRegistryV2Abstract(config) {
  private val logger: Logger = LoggerFactory.getLogger(this.getClass)


  /**
    * Not used as getToken is overridden
    */
  override protected def authorizationServerHostName(dockerImageIdentifier: DockerImageIdentifier): String = {
    logger.warn("A call has been made to authorizationServerHostName for an ECR container but ECR auth should be done via SDK. This call will be ignored")
    ""
  }

  /**
    * Not used as getToken is overridden
    */
  override protected def buildTokenRequestHeaders(dockerInfoContext: DockerInfoActor.DockerInfoContext): List[Header] = {
    logger.warn("A call has been made to buildTokenRequestHeaders for an ECR container but ECR auth should be done via SDK. This call will be ignored")
    List.empty
  }

  /**
    * Advertise every manifest media type in a single request. ECR only returns the stored manifest verbatim
    * (so sha256(body) == ECR's imageDigest) when all types are listed; asking for Docker v2 alone makes ECR
    * convert an OCI manifest and hand back a digest that doesn't exist in the registry, failing the pull.
    */
  override protected def manifestAcceptHeaders: NonEmptyList[Accept] = NonEmptyList.of(AcceptAllManifestsHeader)

  /**
    * ECR has no implicit "library/" namespace: `<registry>/fastqc:tag` lives in repository `fastqc`.
    * The Docker Hub default would request `/v2/library/fastqc/...`, which ECR answers with 404 NAME_UNKNOWN.
    */
  override protected def manifestRepositoryPath(dockerImageID: DockerImageIdentifier): String = dockerImageID.name

  /**
    * Amazon ECR repositories don't have a digest header in responses so we must make it from the manifest body
    */
  override protected def getDigestFromResponse(response: Response[IO]): IO[DockerHashResult] = response match {
    case Status.Successful(r) => digestManifest(r.bodyText)
    // Include ECR's response body: it names the repository / reason, which is otherwise lost in the lookup failure log.
    case Status.Unauthorized(r) => r.as[String].flatMap(body => IO.raiseError(EcrUnauthorized(s"401 Unauthorized: not authorized to obtain a digest from this registry, the token may be invalid. $body")))
    case Status.NotFound(r) => r.as[String].flatMap(body => IO.raiseError(EcrNotFound(s"404 Not Found: $body")))
    case Status.Forbidden(r) => r.as[String].flatMap(body => IO.raiseError(EcrForbidden(s"403 Forbidden: $body")))
    case failed => failed.as[String].flatMap(body => IO.raiseError(new Exception(s"Failed to get manifest: ${failed.status.code} $body")))
  }

  private def digestManifest(bodyText: fs2.Stream[IO, String]): IO[DockerHashResult] = {
    logger.info("computing sha256 digest for container manifest")
    bodyText
      .compile
      .string
      .map(data => "sha256:"+DigestUtils.sha256Hex(data))
      .map(DockerHashResult.fromString)
      .flatMap(IO.fromTry)
  }
}
