package scalafi.garch.estimate

import breeze.linalg.DenseVector
import breeze.linalg.{mean => sampleMean}
import breeze.numerics.abs
import breeze.numerics.sqrt

import scalafi.garch.{Innovations, Mean, Spec}


class ArmaGarchLikelihood[M <: Mean, I <: Innovations](data: DenseVector[Double], spec: Spec[M, I]) {

  import Innovations._
  import Mean._

  case class Parameters(mu: Double, ar: Seq[Double], ma: Seq[Double], omega: Double, alpha: Seq[Double], beta: Seq[Double]) {
    override def toString: String = {
      s"Parameters(mu = $mu, ar = (${ar.mkString(",")}), ma = (${ma.mkString(",")}), omega = $omega, alpha = (${alpha.mkString(",")}), beta = (${beta.mkString(",")}))"
    }
  }

  object Parameters {
    def apply(v: DenseVector[Double]): Parameters = {
      val modelParameters = 1 + spec.mean.arOrder + spec.mean.maOrder + 1 + spec.innovations.alphaOrder + spec.innovations.betaOrder
      assume(
        v.length == modelParameters,
        s"Illegal model parameters length, expected = $modelParameters, got = ${v.length}"
      )
      val p = scala.collection.mutable.ListBuffer(v.activeValuesIterator.toSeq: _*)
      def take(n: Int) = (0 until n).map(_ => p.remove(0)).toSeq
      Parameters(
        take(1).head,
        take(spec.mean.arOrder),
        take(spec.mean.maOrder),
        take(1).head,
        take(spec.innovations.alphaOrder),
        take(spec.innovations.betaOrder)
      )
    }
  }

  // Innovations distribution
  private lazy val unitDistribution = breeze.stats.distributions.Gaussian(mu = 0, sigma = 1)

  private def density(z: Double, hh: Double) = unitDistribution.pdf(z / hh) / hh

  def apply(parameters: DenseVector[Double]): Double = {
    apply(Parameters(parameters))
  }

  def apply(parameters: Parameters): Double = {
    val (err, sigma) = errorsAndSigma(parameters)

    // Calculate log likelihood
    val llh = for (i <- 0 until data.length) yield {
      val e = err(i)
      val s = sigma(i)
      math.log(density(e, s))
    }

    -1 * llh.sum
  }

  def err(parameters: DenseVector[Double]): DenseVector[Double] = err(Parameters(parameters))

  def sigma(parameters: DenseVector[Double]): DenseVector[Double] = sigma(Parameters(parameters))

  def err(parameters: Parameters): DenseVector[Double] = errorsAndSigma(parameters)._1

  def sigma(parameters: Parameters): DenseVector[Double] = errorsAndSigma(parameters)._2

  private def errorsAndSigma(parameters: Parameters): (DenseVector[Double], DenseVector[Double]) = {
    val Parameters(mu, ar, ma, omega, alpha, beta) = parameters

    def initialize() = {
      val err: DenseVector[Double] = data :- mu
      val errSq: DenseVector[Double] = err :^ 2.0

      val x0 = Seq.fill(spec.mean.arOrder)(mu)
      val err0 = Seq.fill(spec.mean.maOrder)(sampleMean(err))
      val errSq0 = Seq.fill(spec.innovations.alphaOrder)(sampleMean(errSq))
      val sigmaSq0 = Seq.fill(spec.innovations.betaOrder)(sampleMean(errSq))

      (x0, err0, errSq0, sigmaSq0)
    }

    // Initialize error & sigma with mean squared error
    val (x0, err0, errSq0, sigmaSq0) = initialize()

    // Accumulate error & associated sigma squared
    val err = DenseVector.zeros[Double](data.length)
    val sigmaSq = DenseVector.zeros[Double](data.length)

    // Initialize ARMA-GARCH recursion
    var _x = x0
    var _err = err0
    var _errSq = errSq0
    var _sigmaSq = sigmaSq0

    def multiply: (Double, Double) => Double = {
      case (d1, d2) => d1 * d2
    }

    for (t <- 0 until data.length) {
      val xT = data(t)
      val meanT = mu + (ar zip _x).map(multiply.tupled).sum + (ma zip _err).map(multiply.tupled).sum
      val errT = xT - meanT
      val sigmaSqT = omega + (alpha zip _errSq).map(multiply.tupled).sum + (beta zip _sigmaSq).map(multiply.tupled).sum

      err.update(t, errT)
      sigmaSq.update(t, sigmaSqT)

      // Update recursion data for next step
      if (!ar.isEmpty)    _x       = xT                +: _x.dropRight(1)
      if (!ma.isEmpty)    _err     = errT              +: _err.dropRight(1)
      if (!alpha.isEmpty) _errSq   = math.pow(errT, 2) +: _errSq.dropRight(1)
      if (!beta.isEmpty)  _sigmaSq = sigmaSqT          +: _sigmaSq.dropRight(1)
    }

    val sigma = sqrt(abs(sigmaSq))

    (err, sigma)
  }
}