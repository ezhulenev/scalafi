package scalafi.garch.estimate

import scalafi.garch.OldGarch
import scalafi.garch.estimate.Optimizer.BreezeOptimizer

trait MaximumLikelihoodEstimate[G <: OldGarch] {
  type Parameters

  def likelihood(params: Parameters): Double

  def estimate(optimizer: Optimizer = BreezeOptimizer): Either[EstimationError, G#Estimate]
}