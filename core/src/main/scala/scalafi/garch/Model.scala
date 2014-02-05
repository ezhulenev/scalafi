package scalafi.garch

trait Model {
  self =>

  type Estimate
}

case class Spec[M <: Mean, I <: Innovations](mean: M, innovations: I) {
  self =>

  case class Estimate(mean: M#Estimate, innovations: I#Estimate)

  case class Forecast(mean: Double, variance: Double)
}