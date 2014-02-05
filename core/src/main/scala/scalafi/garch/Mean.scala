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
  mean =>

  class Estimate(mu: EstimatedValue) extends EstimateLike {
    val estimates: Seq[NamedEstimatedValue] = mu.named("mu") :: Nil

    override def toString: String = s"$mean estimate: ${estimates.mkString(",")}"

  }

}

case class Arma(m: Int, n: Int) extends Mean {
  arma =>

  class Estimate(mu: EstimatedValue, ar: Seq[EstimatedValue], ma: Seq[EstimatedValue]) extends EstimateLike {
    assume(ar.size == m, s"Illegal 'ar' order for model '$arma'")
    assume(ma.size == n, s"Illegal 'ma' order for model '$arma'")

    val estimates: Seq[NamedEstimatedValue] = mu.named("mu") +: ar.named("ar") ++: ma.named("ma")

    override def toString: String = s"$arma estimate: ${estimates.mkString(",")}"
  }

}