package scalafi.garch

sealed trait Innovations extends Model

object Innovations {

  implicit class RichInnovations(val innovations: Innovations) extends AnyVal {
    def alphaOrder = innovations match {
      case Garch(p, _) => p
    }

    def betaOrder = innovations match {
      case Garch(_, q) => q
    }
  }

}


case class Garch(p: Int, q: Int) extends Innovations {
  garch =>

  case class Estimate(omega: EstimatedValue, alpha: Seq[EstimatedValue], beta: Seq[EstimatedValue]) {
    assume(alpha.size == p, s"Illegal 'alpha' order for model '$garch'")
    assume(beta.size == q, s"Illegal 'beta' order for model '$garch'")
  }
}