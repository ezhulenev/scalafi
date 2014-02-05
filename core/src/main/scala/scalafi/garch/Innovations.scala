package scalafi.garch

sealed trait Innovations extends Model {

  case class Forecast(variance: Double) extends ForecastLike

}

object Innovations {

  implicit class RichInnovations(val innovations: Innovations) extends AnyVal {
    def alphaOrder = innovations match {
      case _: Garch111 => 1
    }

    def betaOrder = innovations match {
      case _: Garch111 => 1
    }
  }

}


case class Garch111() extends Innovations {
  garch =>

  case class Estimate(omega: EstimatedValue, alpha: EstimatedValue, beta: EstimatedValue) extends EstimateLike {
    val model: Garch111.this.type = garch
  }

}