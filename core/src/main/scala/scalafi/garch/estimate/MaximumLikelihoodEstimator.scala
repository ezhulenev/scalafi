package scalafi.garch.estimate

import scalafi.garch.estimate.Optimizer.BreezeOptimizer

trait MaximumLikelihoodEstimator {
  type Parameters

  def likelihood(params: Parameters): Double

  def estimate(optimizer: Optimizer = BreezeOptimizer): Either[OptimizationError, Parameters]
}