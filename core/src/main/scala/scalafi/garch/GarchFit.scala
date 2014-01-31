package scalafi.garch


sealed trait GarchFit

object GarchFit {

  case class Garch11Fit(mu: EstimatedValue, omega: EstimatedValue, alpha: EstimatedValue, beta: EstimatedValue) extends GarchFit

}