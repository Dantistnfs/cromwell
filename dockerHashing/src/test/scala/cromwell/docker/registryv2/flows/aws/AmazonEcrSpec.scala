package cromwell.docker.registryv2.flows.aws

import cats.effect.{IO, Resource}
import cromwell.core.TestKitSuite
import cromwell.docker.DockerInfoActor.DockerInfoSuccessResponse
import cromwell.docker.registryv2.DockerRegistryV2Abstract
import cromwell.docker.{DockerImageIdentifier, DockerInfoActor, DockerInfoRequest, DockerRegistryConfig}
import org.apache.commons.codec.digest.DigestUtils
import org.http4s.{AuthScheme, Header, Headers, MediaType, Request, Response}
import org.http4s.client.Client
import org.http4s.headers.`Content-Type`
import org.mockito.Mockito._
import org.scalatest.{BeforeAndAfter, PrivateMethodTester}
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers
import software.amazon.awssdk.services.ecr.EcrClient
import software.amazon.awssdk.services.ecr.model.{AuthorizationData, GetAuthorizationTokenResponse}

class AmazonEcrSpec extends TestKitSuite with AnyFlatSpecLike with Matchers with BeforeAndAfter with PrivateMethodTester{
  behavior of "AmazonEcr"

  val goodUri = "123456789012.dkr.ecr.us-east-1.amazonaws.com/amazonlinux/amazonlinux:latest"
  val otherUri = "ubuntu:latest"

  val mediaType: MediaType = MediaType.parse(DockerRegistryV2Abstract.DockerManifestV2MediaType).getOrElse(fail("Can't parse media type"))
  val contentType: Header = `Content-Type`(mediaType)
  val mockEcrClient: EcrClient = mock(classOf[EcrClient])
  implicit val mockIOClient: Client[IO] = Client({ _: Request[IO] =>
    // This response will have an empty body, so we need to be explicit about the typing:
    Resource.pure[IO, Response[IO]](Response(headers = Headers.of(contentType))) : Resource[IO, Response[IO]]
  })

  val registry = new AmazonEcr(DockerRegistryConfig.default, mockEcrClient)

  it should "accept good URI" in {
    val dockerImageIdentifier = DockerImageIdentifier.fromString(goodUri).get
    registry.accepts(dockerImageIdentifier) shouldEqual true
  }

  it should "NOT accept other URI" in {
    val dockerImageIdentifier = DockerImageIdentifier.fromString(otherUri).get
    registry.accepts(dockerImageIdentifier) shouldEqual false
  }

  it should "use Basic Auth Scheme" in {
    val authSchemeMethod = PrivateMethod[AuthScheme](Symbol("authorizationScheme"))
    registry invokePrivate authSchemeMethod() shouldEqual AuthScheme.Basic
  }

  it should "return 123456789012.dkr.ecr.us-east-1.amazonaws.com as registryHostName" in {
    val registryHostNameMethod = PrivateMethod[String](Symbol("registryHostName"))
    registry invokePrivate registryHostNameMethod(DockerImageIdentifier.fromString(goodUri).get) shouldEqual "123456789012.dkr.ecr.us-east-1.amazonaws.com"
  }

  it should "return expected auth token" in {
    val token = "auth-token"
    val imageId = DockerImageIdentifier.fromString(goodUri).get
    val dockerInfoRequest = DockerInfoRequest(imageId)
    val context = DockerInfoActor.DockerInfoContext(request = dockerInfoRequest, replyTo =  emptyActor)

    when(mockEcrClient.getAuthorizationToken)
      .thenReturn(GetAuthorizationTokenResponse
        .builder()
        .authorizationData(AuthorizationData
          .builder()
          .authorizationToken(token)
          .build())
        .build)

    val getTokenMethod = PrivateMethod[IO[Option[String]]](Symbol("getToken"))
    registry invokePrivate getTokenMethod(context, mockIOClient) ensuring(io => io.unsafeRunSync().get == token)
  }

  it should "build a combined manifest Accept header listing all four media types" in {
    val headerValue = DockerRegistryV2Abstract.AcceptAllManifestsHeader.value
    List(
      DockerRegistryV2Abstract.DockerManifestV2MediaType,
      DockerRegistryV2Abstract.DockerManifestListV2MediaType,
      DockerRegistryV2Abstract.OCIManifestV1MediaType,
      DockerRegistryV2Abstract.OCIIndexV1MediaType
    ).foreach(mediaType => headerValue should include(mediaType))
  }

  it should "send Basic auth and the combined Accept header, then digest the returned OCI manifest body" in {
    val token = "auth-token"
    when(mockEcrClient.getAuthorizationToken)
      .thenReturn(GetAuthorizationTokenResponse
        .builder()
        .authorizationData(AuthorizationData.builder().authorizationToken(token).build())
        .build)

    val body = """{"schemaVersion":2,"mediaType":"application/vnd.oci.image.manifest.v1+json"}"""
    val ociContentType: Header =
      `Content-Type`(MediaType.parse(DockerRegistryV2Abstract.OCIManifestV1MediaType).getOrElse(fail("Can't parse OCI media type")))
    val capturedRequests = scala.collection.mutable.ListBuffer.empty[Request[IO]]
    val capturingClient: Client[IO] = Client({ req: Request[IO] =>
      capturedRequests += req
      Resource.pure[IO, Response[IO]](Response[IO](headers = Headers.of(ociContentType)).withEntity(body)): Resource[IO, Response[IO]]
    })

    val imageId = DockerImageIdentifier.fromString(goodUri).get
    val context = DockerInfoActor.DockerInfoContext(request = DockerInfoRequest(imageId), replyTo = emptyActor)

    val (response, _) = registry.run(context)(capturingClient).unsafeRunSync()

    // ECR auth goes through the SDK, so only the single manifest request should hit the http client
    capturedRequests should have size 1
    val manifestReq = capturedRequests.head

    val authValue = manifestReq.headers.find(_.name.value.equalsIgnoreCase("Authorization")).map(_.value).getOrElse("")
    authValue should startWith("Basic ")

    val acceptValue = manifestReq.headers.find(_.name.value.equalsIgnoreCase("Accept")).map(_.value).getOrElse(fail("No Accept header"))
    List(
      DockerRegistryV2Abstract.DockerManifestV2MediaType,
      DockerRegistryV2Abstract.DockerManifestListV2MediaType,
      DockerRegistryV2Abstract.OCIManifestV1MediaType,
      DockerRegistryV2Abstract.OCIIndexV1MediaType
    ).foreach(mediaType => acceptValue should include(mediaType))

    response shouldBe a[DockerInfoSuccessResponse]
    response.asInstanceOf[DockerInfoSuccessResponse].dockerInformation.dockerHash.algorithmAndHash shouldEqual
      "sha256:" + DigestUtils.sha256Hex(body)
  }
}
