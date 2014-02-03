#ScalaFI

Financial data analysis and market models in Scala language

### Univariate GARCH models
ScalaFI has tools for univariate GARCH modelling (estimating and forecasting). It is written in Scala and use Breeze (https://github.com/scalanlp/breeze) with high performance Netlib-Java (https://github.com/fommil/netlib-java) linear algebra library for computations.

For now it supports only "vanilla" GARCH(1, 1) model with norlmal innovations distribution.

###### Example:
```scala
object GarchEstimation extends App {
  private val log = LoggerFactory.getLogger(this.getClass)

  lazy val returns: Seq[Double] = loadReturns("/dmbp.csv")

  val spec = garch11()

  log.info(s"Fit '$spec' model into returns of size '${returns.length}'")
  val fit = garchFit(spec, DenseVector(returns:_*))

  fit.fold(
    error => log.error(s"Failed to fit model, err = $error"),
    success => log.info(s"Fitted model = '$fit'")
  )

  log.info("10 steps ahead forecast: ")
  val forecast = garchForecast[Garch11](fit.right.get)
  forecast.forecast(10).foreach(v => log.info(v.toString))

  def loadReturns(resource: String) = {
    scala.io.Source.fromInputStream(this.getClass.getResourceAsStream(resource)).
      getLines().drop(1).map(_.split(",")(1).toDouble).toSeq
  }
}
```
