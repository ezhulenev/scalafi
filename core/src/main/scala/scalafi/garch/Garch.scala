package scalafi.garch

import breeze.linalg.DenseVector

sealed trait Garch {
  garch =>

  type Estimate <: EstimateLike
  type Forecast <: ForecastLike

  trait EstimateLike {
    def model: garch.type
    def err: DenseVector[Double]
    def sigma: DenseVector[Double]
  }

  trait ForecastLike {

  }
}

case class Garch11() extends Garch {
  self =>

  case class Estimate(mu: EstimatedValue, omega: EstimatedValue, alpha: EstimatedValue, beta: EstimatedValue,
                      err: DenseVector[Double], sigma: DenseVector[Double]) extends EstimateLike {
    override lazy val model: Garch11.this.type = self
  }

  case class Forecast(mu: Double, sigma: Double) extends ForecastLike

}