package scalafi.example

import breeze.linalg.DenseVector
import org.slf4j.LoggerFactory
import scalafi.garch._
import scalafi.garch.estimate.{Garch11Estimate, Likelihood}

object LikelihoodTest extends App {
  private val log = LoggerFactory.getLogger(this.getClass)

  lazy val returns1: Seq[Double] = loadReturns("/dmbp.csv" /*"/sp500ret.csv"*/)
  lazy val returns2: Seq[Double] = loadReturns("/sp500ret.csv")

  val est1 = new Garch11Estimate(Garch11(), DenseVector(returns1: _*))
  println(est1.likelihood(est1.Parameters(DenseVector(-0.0061903, 0.0107614, 0.1531341, 0.8059737))))

  // Prepare GARCH(1,1) spec
  val spec1 = Spec(ConstantMean(), Garch111())
  val llh1 = new Likelihood(DenseVector(returns1: _*), spec1) {}
  println("PIZDA = " + llh1.Parameters(DenseVector(-0.0061903, 0.0107614, 0.1531341, 0.8059737)))
  println(llh1.likelihood(llh1.Parameters(DenseVector(-0.0061903, 0.0107614, 0.1531341, 0.8059737))))

  /**
  val spec2 = Spec(Arma11(), Garch111())
  val llh2 = LikelihoodObj.likelihood(spec2, DenseVector(returns2: _*))
  println(llh2(DenseVector(8.5888e-05, 8.3463e-01, -8.6528e-01, 1.3614e-06, 8.8177e-02, 9.0427e-01)))
    * */
  // Load returns from resources
  def loadReturns(resource: String) = {
    scala.io.Source.fromInputStream(this.getClass.getResourceAsStream(resource)).
      getLines().drop(1).map(_.split(",")(1).toDouble).toSeq
  }

}
