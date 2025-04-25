package cromwell.backend.impl.aws

import org.slf4j.{Logger, LoggerFactory}
import software.amazon.awssdk.core.interceptor.Context
import software.amazon.awssdk.core.interceptor.ExecutionAttributes
import software.amazon.awssdk.core.interceptor.ExecutionInterceptor
import software.amazon.awssdk.http.SdkHttpResponse

import scala.jdk.CollectionConverters._
import scala.util.Random

/**
 * Custom execution interceptor that marks HTTP 429 "Too Many Requests" responses as retryable.
 * 
 * This interceptor helps AWS SDK clients better handle rate limiting by:
 * 1. Detecting 429 status codes in responses
 * 2. Adding headers to indicate the request should be retried
 * 3. Logging rate limit events for troubleshooting
 * 4. Adding jitter to retry delay to prevent synchronization of retries
 */
class RateLimitRetryInterceptor extends ExecutionInterceptor {
  
  // Track retry attempts for each request
  private val retryAttempts = new java.util.concurrent.ConcurrentHashMap[String, Int]()
  
  // Maximum number of retries per request
  val maxRetries = 15
  
  override def modifyHttpResponse(context: Context.ModifyHttpResponse, 
                                 executionAttributes: ExecutionAttributes): SdkHttpResponse = {
    val sdkResponse = context.httpResponse()
    val requestId = getRequestId(context.httpRequest().toString)
    
    // If the response is a 429, add a header to indicate it should be retried
    if (sdkResponse.statusCode() == 429) {
      // Get current retry count or 0 if this is the first attempt
      val currentAttempts = retryAttempts.getOrDefault(requestId, 0)
      
      // Only retry if we haven't exceeded max retries
      if (currentAttempts < maxRetries) {
        // Increment retry count for this request
        retryAttempts.put(requestId, currentAttempts + 1)
        
        // Add jittered delay to avoid retry storms
        val baseDelay = calculateExponentialBackoff(currentAttempts)
        val jitteredDelay = addJitter(baseDelay)
        
        // Log that we're going to retry with the calculated delay
        RateLimitRetryInterceptor.logger.warn(s"AWS API rate limit (429) encountered. Will retry with delay of ${jitteredDelay}ms. Request: ${context.httpRequest().toString()} | Attempt: ${currentAttempts + 1} of $maxRetries")
        
        // Mark this request as retryable using the SDK's retryable header
        return sdkResponse.toBuilder()
          .putHeader("aws-retry", List("true").asJava)
          .putHeader("aws-retry-delay", List(jitteredDelay.toString).asJava)
          .build()
      } else {
        // Log that we've exceeded max retries
        RateLimitRetryInterceptor.logger.error(s"AWS API rate limit (429) exceeded maximum retry attempts ($maxRetries). Request: ${context.httpRequest().toString()}")
      }
    }
    
    sdkResponse
  }
  
  // Need to use this method as it's called after the response is received but before
  // determining whether to retry
  override def afterExecution(context: Context.AfterExecution, 
                             executionAttributes: ExecutionAttributes): Unit = {
    val response = context.response()
    val sdkHttpResponse = response.sdkHttpResponse()
    val requestId = getRequestId(context.request().toString)
    
    if (sdkHttpResponse != null && sdkHttpResponse.statusCode() == 429) {
      val currentAttempts = retryAttempts.getOrDefault(requestId, 0)
      
      // Additional logging after execution with more details about the AWS service
      val awsService = context.request().getClass.getSimpleName.replace("Request", "")
      RateLimitRetryInterceptor.logger.warn(s"AWS $awsService service rate limited (429). RequestId: $requestId | Attempt: $currentAttempts of $maxRetries")
    } else if (sdkHttpResponse != null && retryAttempts.containsKey(requestId) && sdkHttpResponse.statusCode() >= 200 && sdkHttpResponse.statusCode() < 300) {
      // Log successful request after retries
      val attempts = retryAttempts.get(requestId)
      if (attempts > 0) {
        RateLimitRetryInterceptor.logger.info(s"AWS request succeeded after $attempts retry attempts due to rate limiting")
      }
    }
    
    // Clean up completed requests to avoid memory leaks
    if (sdkHttpResponse != null && sdkHttpResponse.statusCode() != 429) {
      // Discard the return value which is the previous value
      retryAttempts.remove(requestId); ()
    }
  }
  
  // Get a unique identifier for the request
  private def getRequestId(requestString: String): String = {
    // Use a hash of the request as identifier
    requestString.hashCode.toString
  }
  
  // Calculate exponential backoff with a maximum of 60 seconds
  private def calculateExponentialBackoff(attempt: Int): Long = {
    val maxDelay = 60000L // 60 seconds in milliseconds
    val baseDelay = 100L // 100ms base
    val calculatedDelay = baseDelay * Math.pow(2, attempt.toDouble).toLong
    
    // Cap at max delay
    Math.min(calculatedDelay, maxDelay)
  }
  
  // Add jitter to the delay to prevent synchronized retries
  private def addJitter(delay: Long): Long = {
    val jitterFactor = 0.2 // 20% jitter
    val jitterRange = (delay * jitterFactor).toLong
    
    // Apply random jitter between -jitterRange and +jitterRange
    val random = new Random()
    val jitter = random.nextLong() % (jitterRange * 2 + 1) - jitterRange
    
    // Ensure the delay is at least the base delay minus the max jitter
    Math.max(delay + jitter, 1L)
  }
}

/**
 * Companion object for RateLimitRetryInterceptor
 */
object RateLimitRetryInterceptor {
  // Create a logger instance at the companion object level
  val logger: Logger = LoggerFactory.getLogger(classOf[RateLimitRetryInterceptor])
}