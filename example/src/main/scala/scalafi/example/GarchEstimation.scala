package scalafi.example

import breeze.linalg.DenseVector
import org.slf4j.LoggerFactory
import scalafi.garch._
import scalafi.garch.estimate.ArmaGarchEstimate

object GarchEstimation extends App {
  private val log = LoggerFactory.getLogger(this.getClass)

  lazy val returns: Seq[Double] = loadReturns("/dmbp.csv" /*"/sp500ret.csv"*/)

  // Prepare GARCH(1,1) spec
  val spec = Garch11()

  // Fit estimate into data
  log.info(s"Fit '$spec' model into returns of size '${returns.length}'")
  val estimate = garchFit(spec, DenseVector(returns: _*))

  // Check that estimation completed successfully
  estimate.fold(
    error => log.error(s"Failed to fit model, err = $error"),

    estimated => {
      log.info(s"Estimated model = '$estimated'")
      log.info("10 steps ahead forecast: ")
      val forecast = garchForecast[Garch11](estimated)
      forecast.forecast(10).foreach(v => log.info(v.toString))
    }
  )

  // Load returns from resources
  def loadReturns(resource: String) = {
    scala.io.Source.fromInputStream(this.getClass.getResourceAsStream(resource)).
      getLines().drop(1).map(_.split(",")(1).toDouble).toSeq
  }
}
