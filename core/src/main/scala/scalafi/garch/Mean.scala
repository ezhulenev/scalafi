package scalafi.garch

sealed trait Mean extends Model {

  case class Forecast(mean: Double) extends ForecastLike

}

object Mean {

  implicit class RichMean(val mean: Mean) extends AnyVal {
    def arOrder = mean match {
      case _: ConstantMean => 0
      case _: Arma11 => 1
    }

    def maOrder = mean match {
      case _: ConstantMean => 0
      case _: Arma11 => 1
    }
  }

}

case class ConstantMean() extends Mean {
  mean =>

  class Estimate(mu: Double) extends EstimateLike {
    val model: ConstantMean.this.type = mean
  }

}

case class Arma11() extends Mean {
  mean =>

  class Estimate(mu: EstimatedValue, ar: EstimatedValue, ma: EstimatedValue) extends EstimateLike {
    val model: Arma11.this.type = mean
  }

}