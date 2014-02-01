package scalafi.garch.estimate

import breeze.linalg.DenseVector
import org.slf4j.LoggerFactory
import scala.language.implicitConversions
import scalafi.garch.GarchFit.Garch11Fit
import scalafi.garch.GarchSpec.Garch11Spec
import scalafi.garch.estimate.Optimizer.BreezeOptimizer
import scalafi.garch.EstimatedValue


class Garch11Estimator(spec: Garch11Spec, data: Seq[Double]) extends MaximumLikelihoodEstimator[Garch11Spec] with Hessian {

  private val log = LoggerFactory.getLogger(classOf[Garch11Estimator])

  import breeze.stats.DescriptiveStats.{mean => sampleMean, variance => samleVariance}
  import scala.math.abs

  case class Parameters(mu: Double, omega: Double, alpha: Double, beta: Double) {
    def denseVector = DenseVector(mu, omega, alpha, beta)

    def array: Array[Double] = Array[Double](mu, omega, alpha, beta)

    override def toString: String = s"Parameters(mu = $mu, omega = $omega, alpha = $alpha, beta = $beta)"
  }

  object Parameters {
    def apply(point: DenseVector[Double]): Parameters = {
      assume(point.size == 4)
      Parameters(point(0), point(1), point(2), point(3))
    }

    def apply(point: Array[Double]): Parameters = {
      assume(point.size == 4)
      Parameters(point(0), point(1), point(2), point(3))
    }
  }

  // Innovations distribution
  private lazy val unitDistribution = breeze.stats.distributions.Gaussian(mu = 0, sigma = 1)

  // Sample data parameters
  private val bound = 1e-6
  private val mean = sampleMean(data)
  private val variance = samleVariance(data)

  // Lower & Upper bounds for model parameters
  private val lowerBound = Parameters(mu = -10 * abs(mean), omega = bound * bound, alpha = bound, beta = bound)
  private val upperBound = Parameters(mu = 10 * abs(mean), omega = 100 * variance, alpha = 1 - bound, beta = 1 - bound)

  private def density(z: Double, hh: Double) = unitDistribution.pdf(z / hh) / hh

  def likelihood(params: Parameters) = {

    val Parameters(mu, omega, alpha, beta) = params

    val err = data.map(_ - mu)
    val mean = sampleMean(err.map(scala.math.pow(_, 2)))

    // Initialize error & sigma with mean squared error
    val err0 = mean
    val sigmaSq0 = mean

    // Build sigma squared vector
    val builder = scala.collection.mutable.ListBuffer.empty[Double]
    err.foldLeft(err0, sigmaSq0) { case ((_errSq, _sigmaSq), e) =>
        val sigmaSq = omega + alpha * _errSq + beta * _sigmaSq
        builder += sigmaSq
        (math.pow(e, 2), sigmaSq)
    }
    val sigmaSq = builder.toSeq

    // Take square and calculate likelihood
    val sigma = sigmaSq.map(_.abs).map(math.sqrt).toSeq

    val llh = (err zip sigma).map {
      case (e, s) => density(e, s)
    }.map(math.log).sum * -1

    llh
  }


  override def estimate(optimizer: Optimizer = BreezeOptimizer): Either[OptimizationError, Garch11Fit] = {
    val start = Parameters(mu = mean, omega = 0.1 * samleVariance(data), 0.2, 0.5)
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

        Garch11Fit(estimate(_.mu), estimate(_.omega), estimate(_.alpha), estimate(_.beta))
    }
  }
}