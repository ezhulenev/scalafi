package scalafi.garch

import scalafi.garch.GarchFit.Garch11Fit

sealed trait GarchSpec {
  type Fit <: GarchFit
}

object GarchSpec {

  case class Garch11Spec() extends scalafi.garch.GarchSpec {
    override type Fit = Garch11Fit
  }

}