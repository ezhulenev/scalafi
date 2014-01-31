package scalafi.example

import scalafi.garch.estimate.GarchEstimator
import scalafi.garch.GarchModel.Garch
import scalafi.garch.estimate.Optimizer.ApacheMathOptimizer

object GarchEstimation extends App {

  val sp500ret = scala.io.Source.fromInputStream(this.getClass.getResourceAsStream("/sp500ret.csv")).
    getLines().drop(1).map(_.split(",")(1).toDouble).toSeq

  val dmbp = scala.io.Source.fromInputStream(this.getClass.getResourceAsStream("/dmbp.csv")).
    getLines().drop(1).map(_.split(",")(1).toDouble).toSeq

  val estimator = new GarchEstimator(Garch(1, 1), dmbp)

  println(estimator.likelihood(estimator.Parameters(1, 1, 0.1, 0.8)))

  val result = estimator.estimate(ApacheMathOptimizer)

  print(result)
}
