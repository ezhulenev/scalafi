package scalafi.example

import scalafi.garch.estimate.GarchEstimator
import scalafi.garch.GarchModel.Garch

object GarchEstimation extends App {

  val sp500ret = scala.io.Source.fromInputStream(this.getClass.getResourceAsStream("/sp500ret.csv")).
    getLines().drop(1).map(_.split(",")(1).toDouble).toSeq

  val estimator = new GarchEstimator(Garch(1, 1), sp500ret)

  println(estimator.likelihood(estimator.Parameters(1, 1, 0.1, 0.8)))
}
