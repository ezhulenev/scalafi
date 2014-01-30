package scalafi

import scalafi.garch.GarchModel.Garch


package object garch {

  def garch(): GarchModel = garch(1, 1)

  def garch(p: Int, q: Int): GarchModel = GarchModel.Garch(p, q)

  def garchFit[M <: GarchModel](spec: GarchSpec[M], data: Seq[Double]): GarchFit[M] = spec match {
    case GarchSpec.GarchSpec(model@Garch(1, 1)) => ???
    case _ => ???
  }
}
