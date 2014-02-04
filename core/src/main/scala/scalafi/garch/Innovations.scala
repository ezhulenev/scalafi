package scalafi.garch

sealed trait Innovations extends Model {
  case class Forecast(variance: Double) extends ForecastLike
}

case class Garch111() extends Innovations { garch =>

  case class Estimate(omega: EstimatedValue, alpha: EstimatedValue, beta: EstimatedValue) extends EstimateLike {
    val model: Garch111.this.type = garch
  }
}