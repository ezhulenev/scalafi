package scalafi.garch

trait Model {
  self =>

  type Estimate <: EstimateLike
  type Forecast <: ForecastLike

  trait EstimateLike {
    def model: self.type
  }

  trait ForecastLike {

  }
}

case class Spec[M <: Mean, I <: Innovations](mean: M, innovations: I) extends Model {
  self =>

  case class Estimate(mean: M#Estimate, innovations: I#Estimate) extends EstimateLike {
    val model: Spec.this.type = self
  }

  case class Forecast(mean: Double, variance: Double) extends ForecastLike

}