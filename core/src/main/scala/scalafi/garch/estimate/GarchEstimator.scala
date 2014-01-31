package scalafi.garch.estimate

import breeze.linalg.DenseVector
import breeze.optimize.{ApproximateGradientFunction, LBFGS}

import org.apache.commons.math3.analysis.MultivariateFunction
import org.apache.commons.math3.optimization.GoalType
import org.apache.commons.math3.optimization.direct.CMAESOptimizer

import org.slf4j.LoggerFactory

import scala.language.implicitConversions

import scalafi.garch.GarchModel.Garch
import scalafi.garch.estimate.Optimizer.BreezeOptimizer

class GarchEstimator(model: Garch, data: Seq[Double]) extends MaximumLikelihoodEstimator {
  private val log = LoggerFactory.getLogger(classOf[GarchEstimator])

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
  private val s = 1e-6
  private val mean = sampleMean(data)
  private val variance = samleVariance(data)

  // Lower & Upper bounds for model parameters
  private val lowerBound = Parameters(mu = -10 * abs(mean), omega = s * s, alpha = s, beta = s)
  private val upperBound = Parameters(mu = 10 * abs(mean), omega = 100 * variance, alpha = 1 - s, beta = 1 - s)

  private def density(z: Double, hh: Double) = unitDistribution.pdf(z / hh) / hh

  def likelihood(params: Parameters) = {

    val Parameters(mu, omega, alpha, beta) = params

    val z = data.map(_ - mu)
    val mean = sampleMean(z.map(scala.math.pow(_, 2)))

    val e = (mean +: z.dropRight(1).map(math.pow(_, 2))).map(alpha * _).map(_ + omega)

    val h = e.foldLeft(Seq.empty[Double]) {
      case (Nil, v) => (mean * beta + v) :: Nil
      case (seq, v) => seq :+ (seq.last * beta + v)
    }

    val hh = h.map(_.abs).map(math.sqrt).toSeq

    val llh = (z zip hh).map {
      case (_z, _hh) =>
        density(_z, _hh)
    }.map(math.log).sum * -1

    llh
  }

  override def estimate(optimizer: Optimizer = BreezeOptimizer): Either[OptimizationError, Parameters] = {
    val start = Parameters(mu = mean, omega = 0.1 * samleVariance(data), 0.2, 0.5)
    log.debug(s"Start point = $start, likelihood = ${likelihood(start)}")

    def objectiveFunction(params: DenseVector[Double]) = likelihood(Parameters(params))

    val result = optimizer.optimize(objectiveFunction, start.denseVector, Some(lowerBound.denseVector), Some(upperBound.denseVector))

    result.right.map(Parameters.apply)
  }
}