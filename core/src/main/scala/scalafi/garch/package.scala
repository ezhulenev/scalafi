package scalafi

package object garch {

  def garch(): GarchModel = garch(1, 1)

  def garch(p: Int, q: Int): GarchModel = GarchModel.Garch(p, q)
}
