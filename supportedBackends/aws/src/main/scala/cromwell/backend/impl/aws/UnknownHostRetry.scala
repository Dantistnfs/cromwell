package cromwell.backend.impl.aws

import java.net.UnknownHostException

import org.slf4j.{Logger, LoggerFactory}

import scala.annotation.tailrec
import scala.util.{Failure, Random, Success, Try}

/**
 * Helpers for surviving transient DNS-resolution failures when the AWS backend talks to S3 while
 * submitting a job.
 *
 * Motivation (SINFRA orphaned-job incident): when many tasks launch at once, the EC2 VPC DNS
 * resolver can be throttled (hard limit ~1024 packets/s per ENI). A few AWS SDK calls then fail
 * with `java.net.UnknownHostException`. The SDK wraps that in an `SdkClientException`, so the DNS
 * cause is only visible by walking the cause chain, and the SDK's default retry (a few quick
 * attempts) does not outlast a multi-second throttle. When the script-staging GET/HEAD/PUT to the
 * scripts bucket fails this way during submission, the submission is lost and the task is orphaned
 * in "Running" with no AWS Batch jobId.
 *
 * These calls already run on the (serialized) submit path, which blocks on synchronous S3 I/O, so a
 * bounded blocking retry here is consistent with the existing design.
 */
object UnknownHostRetry {

  private val logger: Logger = LoggerFactory.getLogger(getClass)

  /** Max submit-time S3 attempts (1 initial + retries) before giving up and surfacing the failure. */
  val MaxAttempts: Int = 6
  private val BaseBackoffMillis: Long = 200L
  private val MaxBackoffMillis: Long = 5000L

  /**
   * Runs `f`, retrying only when the failure is (or wraps) a DNS-resolution failure. Any other
   * exception is rethrown immediately, so behaviour is unchanged for non-DNS errors. On exhaustion
   * the last DNS failure is rethrown, so the caller can fail the job (which Cromwell then retries as
   * a non-fatal failure) rather than leaving the task orphaned.
   *
   * @param sleep injectable for tests; defaults to a real blocking sleep.
   */
  def withDnsRetry[A](description: String,
                      maxAttempts: Int = MaxAttempts,
                      sleep: Long => Unit = ms => Thread.sleep(ms))(f: => A): A = {
    @tailrec
    def attempt(n: Int): A =
      Try(f) match {
        case Success(a) => a
        case Failure(t) if hasUnknownHostCause(t) && n < maxAttempts =>
          val backoff = backoffMillis(n)
          logger.warn(
            s"Transient DNS resolution failure during '$description' (attempt $n/$maxAttempts); retrying in ${backoff}ms",
            t
          )
          sleep(backoff)
          attempt(n + 1)
        case Failure(t) => throw t
      }

    attempt(1)
  }

  /** Exponential backoff with full jitter, capped at [[MaxBackoffMillis]]. `n` is 1-based. */
  private[aws] def backoffMillis(n: Int): Long = {
    val exp = math.min(BaseBackoffMillis * (1L << (n - 1)), MaxBackoffMillis)
    math.max((Random.nextDouble() * exp).toLong, 1L)
  }

  /**
   * True if any throwable in the cause chain is a DNS-resolution failure ([[UnknownHostException]]).
   * Cloud SDKs commonly wrap these in their own RuntimeException (e.g. AWS's `SdkClientException`),
   * so matching the top-level type alone is not enough. Guards against cyclic cause chains.
   *
   * NB: mirrors `cromwell.engine.io.RetryableRequestSupport.hasUnknownHostCause`; duplicated here
   * because the aws backend module cannot depend on the engine module.
   */
  def hasUnknownHostCause(failure: Throwable): Boolean = {
    @tailrec
    def loop(t: Throwable, seen: Set[Throwable]): Boolean = t match {
      case null => false
      case _ if seen.contains(t) => false // guard against cyclic cause chains
      case _: UnknownHostException => true
      case _ => loop(t.getCause, seen + t)
    }

    loop(failure, Set.empty)
  }
}
