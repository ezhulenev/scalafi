package scalafi.garch

sealed trait GarchModel

object GarchModel {
  case class Garch(p: Int, q: Int) extends GarchModel
}

