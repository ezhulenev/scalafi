package scalafi.garch

import breeze.linalg.DenseVector

sealed trait OldGarch {
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

case class Garch11() extends OldGarch {
  self =>

  case class Estimate(mu: EstimatedValue, omega: EstimatedValue, alpha: EstimatedValue, beta: EstimatedValue,
                      err: DenseVector[Double], sigma: DenseVector[Double]) extends EstimateLike {

    override lazy val model: Garch11.this.type = self

    override def toString: String = s"Garch(1,1) estimate: mu = $mu, omega = $omega, alpha = $alpha, beta = $beta"
  }

  case class Forecast(mu: Double, sigma: Double) extends ForecastLike {
    override def toString: String = s"mean = $mu, sigma = $sigma"
  }

}

case class ArmaGarch() extends OldGarch {
  self =>

  case class Estimate(mu: EstimatedValue,
                      ar: EstimatedValue, ma: EstimatedValue, // ARMA
                      omega: EstimatedValue, alpha: EstimatedValue, beta: EstimatedValue, // GARCH
                      err: DenseVector[Double], sigma: DenseVector[Double]) extends EstimateLike {

    override lazy val model: ArmaGarch.this.type = self

    override def toString: String = s"ArmaGarch estimate: mu = $mu, ar = $ar, ma = $ma, omega = $omega, alpha = $alpha, beta = $beta"
  }

  case class Forecast(mu: Double, sigma: Double) extends ForecastLike {
    override def toString: String = s"mean = $mu, sigma = $sigma"
  }
}