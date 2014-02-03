package scalafi.garch.estimate

import scalafi.garch.Garch
import scalafi.garch.estimate.Optimizer.BreezeOptimizer

trait MaximumLikelihoodEstimate[G <: Garch] {
  type Parameters

  def likelihood(params: Parameters): Double

  def estimate(optimizer: Optimizer = BreezeOptimizer): Either[EstimationError, G#Estimate]
}