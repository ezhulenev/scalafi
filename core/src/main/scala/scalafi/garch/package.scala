package scalafi

import breeze.linalg.DenseVector

import scalafi.garch.estimate._
import scalafi.garch.forecast.Forecast

package object garch {
  
  implicit class RichEstimatedValue(val estimated: EstimatedValue) extends AnyVal {
    def named(name: String) = NamedEstimatedValue(name, estimated)
  } 
  
  implicit class RichEstimatedValues(val estimated: Seq[EstimatedValue]) extends AnyVal {
    def named(name: String) = estimated.zipWithIndex.map {
      case (e, idx) => NamedEstimatedValue(s"$name${idx+1}", e)
    }
  }

  implicit class RichMean[M <: Mean](val mean: M) extends AnyVal {
    def +[I <: Innovations](innovations: I): Spec[M, I] = Spec(mean, innovations)
  }

  def constantMean() = ConstantMean()

  def arma(m: Int, n: Int) = Arma(m, n)

  def garch(p: Int, q: Int) = Garch(1, 1)

  def garchFit[M <: Mean, I <: Innovations](spec: Spec[M, I], data: DenseVector[Double])
                                           (implicit mean: ModelParameters[M], innovations: ModelParameters[I]): Either[EstimationError, Spec[M, I]#Estimate] = {
    val estimator = new Estimator(data, spec)
    estimator.estimate()
  }

  def garchForecast[M <: Mean, I <: Innovations](spec: Spec[M, I], estimate: Spec[M, I]#Estimate): Forecast[M, I] = {
    new Forecast(spec, estimate)
  }
}