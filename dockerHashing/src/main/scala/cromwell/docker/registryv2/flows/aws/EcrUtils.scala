package cromwell.docker.registryv2.flows.aws

object EcrUtils {

  // Pass the message to Exception so it surfaces in "Failed to get docker hash for <image> <message>" (otherwise "null").
  case class EcrUnauthorized(msg: String) extends Exception(msg)
  case class EcrNotFound(msg: String) extends Exception(msg)
  case class EcrForbidden(msg: String) extends Exception(msg)

}
