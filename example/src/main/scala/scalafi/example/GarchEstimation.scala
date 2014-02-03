package scalafi.example

import scalafi.garch._
import org.slf4j.LoggerFactory
import breeze.linalg.DenseVector

object GarchEstimation extends App {
  private val log = LoggerFactory.getLogger(this.getClass)

  //lazy val returns = returns("/sp500ret.csv")
  lazy val returns = loadReturns("/dmbp.csv")

  val spec = garch11()

  log.info(s"Fit '$spec' model into returns of size '${returns.length}'")
  val fit = garchFit(spec, DenseVector(returns:_*))

  fit.fold(
    error => log.error(s"Failed to fit model, err = $error"),
    success => log.info(s"Fitted model = '$fit'")
  )

  log.info("10 steps ahead forecast: ")
  val forecast = garchForecast(fit.right.get)
  forecast.forecast(10).foreach(v => log.info(v.toString))

  def loadReturns(resource: String) = {
    scala.io.Source.fromInputStream(this.getClass.getResourceAsStream(resource)).
      getLines().drop(1).map(_.split(",")(1).toDouble).toSeq
  }
}
