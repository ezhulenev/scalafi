package scalafi.example

import breeze.linalg.DenseVector
import org.slf4j.LoggerFactory
import scalafi.garch._

object GarchEstimation extends App {
  private val log = LoggerFactory.getLogger(this.getClass)

  lazy val dmbp: Seq[Double] = loadReturns("/dmbp.csv")
  lazy val snp: Seq[Double] = loadReturns("/sp500ret.csv")
  
  val returns = dmbp

  val model = constantMean() + garch(1, 1)

  // Fit estimate into data
  log.info(s"Fit '$model' model into returns of size '${returns.length}'")
  val estimate = garchFit(model, DenseVector(returns: _*))

  // Check that estimation completed successfully
  estimate.fold(
    error => log.error(s"Failed to fit model, err = $error"),

    estimated => {
      log.info(s"Estimated model = '$estimated'")
      log.info("10 steps ahead forecast: ")
      val forecast = garchForecast(model, estimated)
      forecast(10).foreach(v => log.info(v.toString))
    }
  )

  // Load returns from resources
  def loadReturns(resource: String) = {
    scala.io.Source.fromInputStream(this.getClass.getResourceAsStream(resource)).
      getLines().drop(1).map(_.split(",")(1).toDouble).toSeq
  }
}
