package cromwell.backend.impl.aws

import java.net.{ConnectException, UnknownHostException}

import cromwell.backend.impl.aws.UnknownHostRetry.{hasUnknownHostCause, withDnsRetry}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import software.amazon.awssdk.core.exception.SdkClientException

class UnknownHostRetrySpec extends AnyFlatSpec with Matchers {

  behavior of "UnknownHostRetry.hasUnknownHostCause"

  it should "detect a bare UnknownHostException" in {
    hasUnknownHostCause(new UnknownHostException("congee-scratch-prd.s3.us-west-2.amazonaws.com")) shouldBe true
  }

  it should "detect a DNS failure wrapped in AWS SdkClientException (the reported incident shape)" in {
    // Reproduces the prod stacktrace: an S3 DNS resolution error surfaces as an SdkClientException
    // (a RuntimeException) whose root cause is a java.net.UnknownHostException.
    val host = "congee-scratch-prd.s3.us-west-2.amazonaws.com"
    val inner = SdkClientException
      .builder()
      .message(s"Unable to execute HTTP request: $host (SDK Attempt Count: 3)")
      .cause(new UnknownHostException(host))
      .build()
    val outer = SdkClientException
      .builder()
      .message("Received an UnknownHostException when attempting to interact with a service.")
      .cause(inner)
      .build()

    hasUnknownHostCause(outer) shouldBe true
  }

  it should "not match non-DNS network errors" in {
    hasUnknownHostCause(new RuntimeException(new ConnectException("connection refused"))) shouldBe false
  }

  it should "not match a plain non-network exception" in {
    hasUnknownHostCause(new IllegalArgumentException("nope")) shouldBe false
  }

  it should "not loop forever on a cyclic cause chain" in {
    val a = new RuntimeException("a")
    val b = new RuntimeException("b")
    a.initCause(b)
    b.initCause(a) // a -> b -> a
    hasUnknownHostCause(a) shouldBe false
  }

  behavior of "UnknownHostRetry.withDnsRetry"

  private val noSleep: Long => Unit = _ => ()

  it should "return immediately on success without retrying" in {
    var calls = 0
    val result = withDnsRetry("op", sleep = noSleep) { calls += 1; 42 }
    calls shouldBe 1
    result shouldBe 42
  }

  it should "retry a transient DNS failure and then succeed" in {
    var calls = 0
    val result = withDnsRetry("op", sleep = noSleep) {
      calls += 1
      if (calls < 3) throw new UnknownHostException("host") else "ok"
    }
    calls shouldBe 3
    result shouldBe "ok"
  }

  it should "retry a DNS failure wrapped in SdkClientException" in {
    var calls = 0
    val result = withDnsRetry("op", sleep = noSleep) {
      calls += 1
      if (calls < 2)
        throw SdkClientException.builder().message("wrapped").cause(new UnknownHostException("host")).build()
      else "ok"
    }
    calls shouldBe 2
    result shouldBe "ok"
  }

  it should "give up after maxAttempts and rethrow the DNS failure" in {
    var calls = 0
    a[UnknownHostException] should be thrownBy withDnsRetry("op", maxAttempts = 4, sleep = noSleep) {
      calls += 1
      throw new UnknownHostException("host")
    }
    calls shouldBe 4
  }

  it should "not retry a non-DNS failure" in {
    var calls = 0
    an[IllegalStateException] should be thrownBy withDnsRetry("op", sleep = noSleep) {
      calls += 1
      throw new IllegalStateException("nope")
    }
    calls shouldBe 1
  }
}
