package scalafi.garch.estimate

trait MaximumLikelihoodEstimator {
  type Parameters
  
  def likelihood(params: Parameters): Double
}