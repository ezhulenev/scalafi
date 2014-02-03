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

###### Output:
```
Garch(1,1) estimate: 
mu    = EstimatedValue(-0.005956513946539034,0.008458876964391205,-0.7041731392493107) 
omega = EstimatedValue(0.010764079956857832,0.002849191762009551,3.7779415553503726) 
alpha = EstimatedValue(0.1537363542775121,0.026664325022776613,5.765619573950993) 
beta  = EstimatedValue(0.8055765724871476,0.033591755543296166,23.981377557027226) 

10 steps ahead forecast:                                                                                    
mean = -0.005956513946539034, sigma = 0.3835356133325803                                                                 
mean = -0.005956513946539034, sigma = 0.389716045099858                                                                  
mean = -0.005956513946539034, sigma = 0.39555426961998585                                                                
mean = -0.005956513946539034, sigma = 0.4010750943394923                                                                 
mean = -0.005956513946539034, sigma = 0.4063007981258263                                                                 
mean = -0.005956513946539034, sigma = 0.411251483510917                                                                  
mean = -0.005956513946539034, sigma = 0.4159453668047668                                                                 
mean = -0.005956513946539034, sigma = 0.42039901912287936                                                                
mean = -0.005956513946539034, sigma = 0.4246275682366235                                                                 
mean = -0.005956513946539034, sigma = 0.42864486886165126                                                                
```
