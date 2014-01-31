package scalafi.example

import scalafi.garch._
import org.slf4j.LoggerFactory

object GarchEstimation extends App {
  private val log = LoggerFactory.getLogger(this.getClass)

  //lazy val returns = returns("/sp500ret.csv")
  lazy val returns = loadReturns("/dmbp.csv")

  val spec = garch11()

  log.info(s"Fit '$spec' model into returns of size '${returns.length}'")
  val fit = garchFit(spec, returns)

  fit.fold(
    error => log.error(s"Failed to fit model, err = $error"),
    success => log.info(s"Fitted model = '$fit'")
  )

  def loadReturns(resource: String) = {
    scala.io.Source.fromInputStream(this.getClass.getResourceAsStream(resource)).
      getLines().drop(1).map(_.split(",")(1).toDouble).toSeq
  }

}
