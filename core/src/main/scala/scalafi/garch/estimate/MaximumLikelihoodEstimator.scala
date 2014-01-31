package scalafi.garch.estimate

import scalafi.garch.GarchSpec
import scalafi.garch.estimate.Optimizer.BreezeOptimizer

trait MaximumLikelihoodEstimator[S <: GarchSpec] {
  type Parameters

  def likelihood(params: Parameters): Double

  def estimate(optimizer: Optimizer = BreezeOptimizer): Either[OptimizationError, S#Fit]
}