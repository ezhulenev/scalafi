package scalafi.garch

sealed trait Mean extends Model

object Mean {

  implicit class RichMean(val mean: Mean) extends AnyVal {
    def arOrder = mean match {
      case _: ConstantMean => 0
      case Arma(ar, _) => ar
    }

    def maOrder = mean match {
      case _: ConstantMean => 0
      case Arma(_, ma) => ma
    }
  }

}

case class ConstantMean() extends Mean {
  class Estimate(mu: EstimatedValue)

}

case class Arma(m: Int, n: Int) extends Mean {
  arma =>

  class Estimate(mu: EstimatedValue, ar: Seq[EstimatedValue], ma: Seq[EstimatedValue]) {
    assume(ar.size == m, s"Illegal 'ar' order for model '$arma'")
    assume(ma.size == n, s"Illegal 'ma' order for model '$arma'")
  }

}