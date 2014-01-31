package scalafi.garch.estimate

sealed trait OptimizationError

object OptimizationError {

  case class OptimizationFailed(cause: Throwable) extends OptimizationError

}
