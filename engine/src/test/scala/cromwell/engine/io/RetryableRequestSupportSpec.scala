package cromwell.engine.io

import java.io.IOException
import java.net.{ConnectException, UnknownHostException}

import cromwell.engine.io.RetryableRequestSupport.{hasUnknownHostCause, isRetryable}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import software.amazon.awssdk.core.exception.SdkClientException

class RetryableRequestSupportSpec extends AnyFlatSpec with Matchers {

  behavior of "RetryableRequestSupport"

  it should "retry a bare UnknownHostException" in {
    isRetryable(new UnknownHostException("congee-scratch-prd.s3.us-west-2.amazonaws.com")) shouldBe true
  }

  it should "retry a transient DNS failure wrapped in AWS SdkClientException (SINFRA)" in {
    // Reproduces the reported failure: an AWS S3 DNS resolution error surfaces as an SdkClientException
    // (a RuntimeException) whose root cause is a java.net.UnknownHostException.
    val host = "congee-scratch-prd.s3.us-west-2.amazonaws.com"
    val root = new UnknownHostException(host)
    val inner = SdkClientException
      .builder()
      .message(s"Unable to execute HTTP request: $host (SDK Attempt Count: 3)")
      .cause(root)
      .build()
    val outer = SdkClientException
      .builder()
      .message("Received an UnknownHostException when attempting to interact with a service.")
      .cause(inner)
      .build()

    isRetryable(outer) shouldBe true
  }

  it should "stay targeted: not retry non-DNS network errors wrapped in a RuntimeException" in {
    // Deliberately narrow. A ConnectException wrapped in a plain RuntimeException is not our reported
    // case; broadening the cause-chain walk to it risks unintended side effects, so it is NOT retried.
    isRetryable(new RuntimeException(new ConnectException("connection refused"))) shouldBe false
  }

  it should "not retry a non-network RuntimeException" in {
    isRetryable(new RuntimeException("bad request")) shouldBe false
    isRetryable(new IllegalArgumentException("nope")) shouldBe false
  }

  it should "not loop forever on a cyclic cause chain" in {
    val a = new RuntimeException("a")
    val b = new RuntimeException("b")
    a.initCause(b)
    b.initCause(a) // a -> b -> a
    hasUnknownHostCause(a) shouldBe false
  }

  it should "still treat generic IOExceptions as non-retryable" in {
    isRetryable(new IOException("some other io problem")) shouldBe false
  }
}
