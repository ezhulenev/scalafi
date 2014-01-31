package scalafi.example

import scalafi.garch.GarchSpec.Garch11Spec

object GarchEstimation extends App {

  import scalafi.garch._

  val sp500ret = scala.io.Source.fromInputStream(this.getClass.getResourceAsStream("/sp500ret.csv")).
    getLines().drop(1).map(_.split(",")(1).toDouble).toSeq

  val dmbp = scala.io.Source.fromInputStream(this.getClass.getResourceAsStream("/dmbp.csv")).
    getLines().drop(1).map(_.split(",")(1).toDouble).toSeq

  val fit = garchFit(Garch11Spec(), dmbp)

  print(fit)
}
