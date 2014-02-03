package scalafi

import breeze.linalg.DenseVector
import scalafi.garch.estimate.{EstimationError, Garch11Estimate, MaximumLikelihoodEstimate}
import scalafi.garch.forecast.{Garch11Forecast, Forecast}


package object garch {

  sealed trait EstimateAux[G <: Garch] {
    def estimate(spec: G, data: DenseVector[Double]): MaximumLikelihoodEstimate[G]
  }

  sealed trait ForecastAux[G <: Garch] {
    def forecast(estimate: G#Estimate): Forecast[G]
  }

  implicit object Garch11EstimateAux extends EstimateAux[Garch11] {
    override def estimate(spec: Garch11, data: DenseVector[Double]): MaximumLikelihoodEstimate[Garch11] = {
      new Garch11Estimate(spec, data)
    }
  }

  implicit def Garch11ForecastAux = new ForecastAux[Garch11] {
    override def forecast(estimate: Garch11#Estimate): Forecast[Garch11] = {
      new Garch11Forecast(estimate)
    }
  }

  def garch11() = Garch11()

  def garchFit[G <: Garch](spec: G, data: DenseVector[Double])(implicit ev: EstimateAux[G]): Either[EstimationError, G#Estimate] = {
    ev.estimate(spec, data).estimate()
  }

  def garchForecast[G <: Garch](estimate: G#Estimate)(implicit ev: ForecastAux[G]): Forecast[G] = {
    ev.forecast(estimate)
  }
}