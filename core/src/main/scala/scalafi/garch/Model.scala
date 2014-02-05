package scalafi.garch

import breeze.linalg.DenseVector

trait Model {
  self =>

  type Estimate <: EstimateLike

  trait EstimateLike {
    def estimates: Seq[NamedEstimatedValue]
  }

}

case class Spec[M <: Mean, I <: Innovations](mean: M, innovations: I) {
  self =>

  case class Estimate(mean: M#Estimate, innovations: I#Estimate,
                      data: DenseVector[Double], err: DenseVector[Double], sigma: DenseVector[Double]) {
    lazy val estimates: Seq[NamedEstimatedValue] = mean.estimates ++: innovations.estimates

    override def toString: String = s"$self estimate: ${estimates.mkString(",")}"
  }

  case class Forecast(mean: Double, variance: Double)

}