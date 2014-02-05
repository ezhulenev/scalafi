#ScalaFI

Financial data analysis and market models in Scala language

### Univariate GARCH models
ScalaFI has tools for univariate GARCH modelling (estimating and forecasting). It is written in Scala and use Breeze (https://github.com/scalanlp/breeze) with high performance Netlib-Java (https://github.com/fommil/netlib-java) linear algebra library for computations.

###### 'Vanilla' GARCH(1,1) Example:
```scala
object GarchEstimation extends App {
  private val log = LoggerFactory.getLogger(this.getClass)

  val returns: Seq[Double] = loadReturns("/dmbp.csv")

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

```

###### Output:
```
Model(mean = ConstantMean(), innovations = Garch(1,1)) estimate:
mu     = ^(-0.00623, se = 0.00846, t-value = -0.73649)
omega  = ^(0.01074,  se = 0.00285, t-value = 3.77318)
alpha1 = ^(0.15300,  se = 0.02650, t-value = 5.77328)
beta1  = ^(0.80619,  se = 0.03351, t-value = 24.05904)

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
