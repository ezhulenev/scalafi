package scalafi.garch.estimate

import breeze.linalg._
import org.slf4j.LoggerFactory
import scala.Some
import scalafi.garch.{EstimatedValue, ArmaGarch}
import scalafi.garch.estimate.Optimizer.{ApacheMathOptimizer, BreezeOptimizer}

class ArmaGarchEstimate(spec: ArmaGarch, data: DenseVector[Double]) extends MaximumLikelihoodEstimate[ArmaGarch] with Hessian {

  private val log = LoggerFactory.getLogger(classOf[ArmaGarchEstimate])

  import breeze.linalg.{mean => sampleMean, variance => samleVariance}
  import breeze.numerics.abs
  import breeze.numerics.sqrt

  case class Parameters(mu: Double, ar: Double, ma: Double, omega: Double, alpha: Double, beta: Double) {
    def denseVector = DenseVector(mu, ar, ma, omega, alpha, beta)

    def array: Array[Double] = Array[Double](mu, omega, alpha, beta)

    override def toString: String = s"Parameters(mu = $mu, ar = $ar, ma = $ma, omega = $omega, alpha = $alpha, beta = $beta)"
  }

  object Parameters {
    def apply(point: DenseVector[Double]): Parameters = {
      assume(point.size == 6)
      Parameters(point(0), point(1), point(2), point(3), point(4), point(5))
    }

    def apply(point: Array[Double]): Parameters = {
      assume(point.size == 6)
      Parameters(point(0), point(1), point(2), point(3), point(4), point(5))
    }
  }

  // Innovations distribution
  private lazy val unitDistribution = breeze.stats.distributions.Gaussian(mu = 0, sigma = 1)

  // Sample data parameters
  private val bound = 1e-6
  private val mean = sampleMean(data)
  private val variance = samleVariance(data)

  // Lower & Upper bounds for model parameters
  private val lowerBound = Parameters(mu = -10 * abs(mean), ar = -1, ma = -1, omega = bound * bound, alpha = bound, beta = bound)
  private val upperBound = Parameters(mu = 10 * abs(mean), ar = 1, ma = 1, omega = 100 * variance, alpha = 1 - bound, beta = 1 - bound)

  private def density(z: Double, hh: Double) = unitDistribution.pdf(z / hh) / hh

  private def likelihoodParams(params: Parameters): (DenseVector[Double], DenseVector[Double]) = {
    val Parameters(mu, ar, ma, omega, alpha, beta) = params

    def initialize() = {
      val err: DenseVector[Double] = data :- mu
      val errSq: DenseVector[Double] = err :^ 2.0

      (mu, sampleMean(err), sampleMean(errSq), sampleMean(errSq))
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

    for (i <- 0 until data.length) {
      val meanT = mu + (ar * _x) + (ma * _err)
      val errT = data(i) - meanT

      err.update(i, errT)
      sigmaSq.update(i, omega + alpha * _errSq + beta * _sigmaSq)

      _x = data(i)
      _err = errT
      _errSq = math.pow(errT, 2)
      _sigmaSq = sigmaSq(i)
    }

    // Take square and calculate likelihood
    val sigma = sqrt(abs(sigmaSq))

    (err, sigma)
  }

  def likelihood(params: Parameters) = {
    val (err, sigma) = likelihoodParams(params)

    // Calculate log likelihood
    val llh = for (i <- 0 until data.length) yield {
      val e = err(i)
      val s = sigma(i)
      math.log(density(e, s))
    }

    -1 * llh.sum
  }

  override def estimate(optimizer: Optimizer = BreezeOptimizer): Either[EstimationError, ArmaGarch#Estimate] = {
    val start = Parameters(mu = mean, ar = 0, ma = 0, omega = 0.1 * samleVariance(data), 0.2, 0.5)
    log.debug(s"Start point = $start, likelihood = ${likelihood(start)}")

    def objectiveFunction(params: DenseVector[Double]) = likelihood(Parameters(params))

    val result = optimizer.optimize(objectiveFunction, start.denseVector, Some(lowerBound.denseVector), Some(upperBound.denseVector))

    result.right.map {
      case output =>
        import breeze.linalg.{diag, inv}
        import breeze.numerics.sqrt

        val end = Parameters(output)
        log.debug(s"End point = $end, likelihood = ${likelihood(end)}")

        val H = hessian(objectiveFunction, output)

        val stdErrors = sqrt(diag(inv(H)))
        val tValues = output.copy /= stdErrors

        val v = Parameters(output)
        val se = Parameters(stdErrors)
        val t = Parameters(tValues)

        def estimate(f: Parameters => Double) = EstimatedValue(f(v), f(se), f(t))

        val (err, sigma) = likelihoodParams(end)

        spec.Estimate(
          estimate(_.mu),
          estimate(_.ar), estimate(_.ma),
          estimate(_.omega), estimate(_.alpha), estimate(_.beta),
          err, sigma
        )
    }
  }
}