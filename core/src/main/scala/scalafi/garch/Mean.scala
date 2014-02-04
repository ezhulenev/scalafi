package scalafi.garch

sealed trait Mean extends Model {
  case class Forecast(mean: Double) extends ForecastLike
}

case class ConstantMean() extends Mean { mean =>

  class Estimate(mu: Double) extends EstimateLike {
    val model: ConstantMean.this.type = mean
  }
}

case class Arma11() extends Mean { mean =>

  class Estimate(mu: EstimatedValue, ar: EstimatedValue, ma: EstimatedValue) extends EstimateLike {
    val model: Arma11.this.type = mean
  }

}