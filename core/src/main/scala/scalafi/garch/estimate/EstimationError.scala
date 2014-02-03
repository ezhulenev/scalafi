package scalafi.garch.estimate

sealed trait EstimationError

object EstimationError {

  case class OptimizationFailed(cause: Throwable) extends EstimationError

}
