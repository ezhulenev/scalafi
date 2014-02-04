package scalafi.example

import org.slf4j.LoggerFactory
import scalafi.garch._
import scalafi.garch.estimate.{LikelihoodObj, Likelihood}
import breeze.linalg.DenseVector

object LikelihoodTest extends App {
  private val log = LoggerFactory.getLogger(this.getClass)

  lazy val returns: Seq[Double] = loadReturns("/dmbp.csv" /*"/sp500ret.csv"*/)

  // Prepare GARCH(1,1) spec
  val spec = Spec(ConstantMean(), Garch111())

  val llh = LikelihoodObj.likelihood(spec, DenseVector(returns: _*))

  println(llh(DenseVector(-0.0061903, 0.0107614, 0.1531341, 0.8059737)))

  // Load returns from resources
  def loadReturns(resource: String) = {
    scala.io.Source.fromInputStream(this.getClass.getResourceAsStream(resource)).
      getLines().drop(1).map(_.split(",")(1).toDouble).toSeq
  }

}
