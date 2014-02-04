package scalafi.garch.estimate

import breeze.linalg._
import scalafi.garch._

trait Likelihood {
  def apply(params: DenseVector[Double]): Double
}

sealed trait LikelihoodCalc[M <: Mean, I <: Innovations] {
  // Mean & Innovations model parameters
  type MeanP
  type InnovationsP

  // Recursion data for Mean & Innovations models
  type MeanRD <: MeanRDLike
  type InnovationsRD <: InnovationsRDLike

  trait MeanRDLike {
    def next(x: Double, err: Double): MeanRD
  }

  trait InnovationsRDLike {
    def next(err: Double, sigmaSq: Double): InnovationsRD
  }

  // Extract parameters from DenseVector
  def parameters(params: DenseVector[Double]): (MeanP, InnovationsP)

  // Initialize recursion steps
  def initialize(mp: MeanP, ip: InnovationsP, data: DenseVector[Double]): (MeanRD, InnovationsRD)

  // Calculate mean & sigmaSq from previous step data
  def mean(p: MeanP, d: MeanRD): Double

  def sigmaSq(p: InnovationsP, d: InnovationsRD): Double
}

object LikelihoodCalc {

  implicit object constantGarch11Calc extends LikelihoodCalc[ConstantMean, Garch111] {

    case class MeanP(mu: Double)

    case class InnovationsP(omega: Double, alpha: Double, beta: Double)

    case class MeanRD(mu: Double) extends MeanRDLike {
      override def next(x: Double, err: Double): MeanRD = MeanRD(mu)
    }

    case class InnovationsRD(errSq: Double, sigmaSq: Double) extends InnovationsRDLike {
      override def next(err: Double, sigmaSq: Double): InnovationsRD = InnovationsRD(math.pow(err, 2), sigmaSq)
    }

    override def sigmaSq(params: InnovationsP, data: InnovationsRD): Double = {
      params.omega + params.alpha * data.errSq + params.beta * data.sigmaSq
    }

    override def mean(params: MeanP, data: MeanRD): Double = params.mu

    override def initialize(mp: MeanP, ip: InnovationsP, data: DenseVector[Double]): (MeanRD, InnovationsRD) = {
      import breeze.linalg.{mean => sampleMean}
      val err: DenseVector[Double] = data :- mp.mu
      val errSq: DenseVector[Double] = err :^ 2.0

      val meanErrSq = sampleMean(errSq)

      (MeanRD(mp.mu), InnovationsRD(meanErrSq, meanErrSq))
    }


    override def parameters(params: DenseVector[Double]): (MeanP, InnovationsP) = {
      assume(params.activeSize == 4)
      (MeanP(params(0)), InnovationsP(params(1), params(2), params(3)))
    }

  }

  implicit object armaGarchCalc extends LikelihoodCalc[Arma11, Garch111] {
    case class MeanP(mu: Double, ar: Double, ma: Double)
    case class InnovationsP(omega: Double, alpha: Double, beta: Double)

    case class MeanRD(mu: Double, x: Double, err: Double) extends MeanRDLike {
      override def next(_x: Double, _err: Double): MeanRD = MeanRD(mu, _x, _err)
    }
    case class InnovationsRD(errSq: Double, sigmaSq: Double) extends InnovationsRDLike {
      override def next(err: Double, sigmaSq: Double): InnovationsRD = InnovationsRD(math.pow(err, 2), sigmaSq)
    }

    override def sigmaSq(params: InnovationsP, data: InnovationsRD): Double = {
      params.omega + params.alpha * data.errSq + params.beta * data.sigmaSq
    }

    override def mean(params: MeanP, data: MeanRD): Double = {
      params.mu + params.ar * data.x + params.ma * data.err
    }

    override def initialize(mp: MeanP, ip: InnovationsP, data: DenseVector[Double]): (MeanRD, InnovationsRD) = {
      import breeze.linalg.{mean => sampleMean}
      val err: DenseVector[Double] = data :- mp.mu
      val errSq: DenseVector[Double] = err :^ 2.0

      val meanErr = sampleMean(err)
      val meanErrSq = sampleMean(errSq)

      (MeanRD(mp.mu, mp.mu, meanErr), InnovationsRD(meanErrSq, meanErrSq))
    }

    override def parameters(params: DenseVector[Double]): (MeanP, InnovationsP) = {
      assume(params.activeSize == 6)
      (MeanP(params(0), params(1), params(2)), InnovationsP(params(3), params(4), params(5)))
    }

  }

}

object LikelihoodObj {

  import breeze.linalg._
  import breeze.numerics._


  // Innovations distribution
  private lazy val unitDistribution = breeze.stats.distributions.Gaussian(mu = 0, sigma = 1)

  private def density(z: Double, hh: Double) = unitDistribution.pdf(z / hh) / hh

  def likelihood[M <: Mean, I <: Innovations](spec: Spec[M, I], data: DenseVector[Double])
                                             (implicit ev: LikelihoodCalc[M, I]) = new Likelihood {

    override def apply(parameters: DenseVector[Double]): Double = {

      val (meanParams, innovationsParams) = ev.parameters(parameters)

      // Initial parameters for mean & variance recursion
      val (mean0, innovations0) = ev.initialize(meanParams, innovationsParams, data)

      // Accumulate error & associated sigma squared
      val err = DenseVector.zeros[Double](data.length)
      val sigmaSq = DenseVector.zeros[Double](data.length)

      // Initialize recursion
      var _meanStep = mean0
      var _innovationsStep = innovations0

      // Loop over data points
      for (t <- 0 until data.length) {
        val xt = data(t)
        val meanT = ev.mean(meanParams, _meanStep)
        val errT = xt - meanT
        val sigmaSqT = ev.sigmaSq(innovationsParams, _innovationsStep)

        err.update(t, errT)
        sigmaSq.update(t, sigmaSqT)

        _meanStep = _meanStep.next(xt, errT)
        _innovationsStep = _innovationsStep.next(errT, sigmaSqT)

      }

      // Take square and calculate likelihood
      val sigma = sqrt(abs(sigmaSq))

      // Calculate log likelihood
      val llh = for (i <- 0 until data.length) yield {
        val e = err(i)
        val s = sigma(i)
        math.log(density(e, s))
      }

      -1 * llh.sum
    }
  }
}