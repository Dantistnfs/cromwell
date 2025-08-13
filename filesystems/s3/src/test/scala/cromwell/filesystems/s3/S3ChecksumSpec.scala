package cromwell.filesystems.s3

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.mockito.Mockito._
import software.amazon.awssdk.services.s3.model.HeadObjectResponse

class S3ChecksumSpec extends AnyFlatSpec with Matchers {
  "S3BatchChecksumCommand" should "prefer CRC64NVME over eTag" in {
    // Mock a HeadObjectResponse returning CRC64NVME and eTag
    val response = HeadObjectResponse.builder()
      .checksumCRC64NVME("deadbeefCRC64")
      .eTag("etag123")
      .build()

    // Dummy S3Path (not used in mapResponse logic)
    val dummyPath = mock(classOf[S3Path])

    val cmd = cromwell.filesystems.s3.batch.S3BatchChecksumCommand(dummyPath)
    val result = cmd.mapResponse(response)

    result shouldEqual "deadbeefCRC64"
  }
} 