package scalafi.example

import breeze.linalg.DenseVector
import org.slf4j.LoggerFactory
import scalafi.garch._
import scalafi.garch.estimate.{Estimator, Garch11Estimate, ArmaGarchLikelihood}
import scalafi.garch.forecast.Forecasting

object LikelihoodTest extends App {
  private val log = LoggerFactory.getLogger(this.getClass)

  lazy val dmbp: Seq[Double] = loadReturns("/dmbp.csv" /*"/sp500ret.csv"*/)
  lazy val sp500: Seq[Double] = loadReturns("/sp500ret.csv")

  val est1 = new Garch11Estimate(Garch11(), DenseVector(dmbp: _*))
  println(est1.likelihood(est1.Parameters(DenseVector(-0.0061903, 0.0107614, 0.1531341, 0.8059737))))

  // Prepare GARCH(1,1) spec
  val spec1 = Spec(ConstantMean(), Garch(1, 1))
  val llh1 = new ArmaGarchLikelihood(DenseVector(dmbp: _*), spec1) {}
  println(llh1.apply(llh1.Parameters(DenseVector(-0.0061903, 0.0107614, 0.1531341, 0.8059737))))

  val spec2 = Spec(Arma(1, 1), Garch(1, 1))
  val llh2 = new ArmaGarchLikelihood(DenseVector(sp500: _*), spec2) {}
  println(llh2.apply(llh2.Parameters(DenseVector(8.5888e-05, 8.3463e-01, -8.6528e-01, 1.3614e-06, 8.8177e-02, 9.0427e-01))))

  val spec3 = Spec(Arma(2, 1), Garch(1, 1))
  val llh3 = new ArmaGarchLikelihood(DenseVector(sp500: _*), spec3) {}
  println(llh3.apply(llh3.Parameters(DenseVector(1.0457e-04, 8.2335e-01, -2.3861e-02, -8.3632e-01, 1.3609e-06, 8.8059e-02, 9.0438e-01))))

  val spec4 = Spec(Arma(2, 1), Garch(2, 1))
  val llh4 = new ArmaGarchLikelihood(DenseVector(sp500: _*), spec4) {}
  println(llh4.apply(llh4.Parameters(DenseVector(1.0443e-04, 8.2354e-01, -2.3827e-02, -8.3653e-01, 1.3594e-06, 8.7986e-02, 1.0000e-08, 9.0445e-01))))

  val spec = spec1
  val estimator = new Estimator(DenseVector(dmbp: _*), spec)
  val estimate = estimator.estimate().right.get
  println("Estimate = "+estimate)
  val forecasting = new Forecasting(spec, estimate)
  val forecast = forecasting.apply(10)
  forecast.foreach(println)

  // Load returns from resources
  def loadReturns(resource: String) = {
    scala.io.Source.fromInputStream(this.getClass.getResourceAsStream(resource)).
      getLines().drop(1).map(_.split(",")(1).toDouble).toSeq
  }

}
