package scalafi

import scalafi.garch.GarchSpec.Garch11Spec
import scalafi.garch.estimate.{OptimizationError, Garch11Estimator, MaximumLikelihoodEstimator}


package object garch {

  sealed trait EstimatorAux[S <: GarchSpec] {
  def apply(spec: S, data: Seq[Double]): MaximumLikelihoodEstimator[S]
  }

  implicit object Garch11Aux extends EstimatorAux[Garch11Spec] {
    override def apply(spec: Garch11Spec, data: Seq[Double]): Garch11Estimator = {
      new Garch11Estimator(spec, data)
    }
  }

  def garch11() = Garch11Spec()

  def garchFit[S <: GarchSpec](spec: S, data: Seq[Double])(implicit ev: EstimatorAux[S]): Either[OptimizationError, S#Fit] = {
    ev(spec, data).estimate()
  }
}