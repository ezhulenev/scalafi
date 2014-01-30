package scalafi.garch

import scalafi.garch.GarchModel.Garch

sealed trait GarchSpec[M <: GarchModel] {
  def model: M
}

object GarchSpec {

  case class GarchSpec(model: Garch) extends scalafi.garch.GarchSpec[Garch]

}