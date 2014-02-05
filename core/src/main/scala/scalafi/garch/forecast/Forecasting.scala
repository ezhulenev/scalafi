package scalafi.garch.forecast

import scalafi.garch._
import breeze.linalg._
import scalafi.garch.Spec

class Forecasting[M <: Mean, I <: Innovations](spec: Spec[M, I], estimate: Spec[M, I]#Estimate) {
  type Forecast = Spec[M, I]#Forecast

  import scalafi.garch.Mean._
  import scalafi.garch.Innovations._

  case class Parameters(mu: Double, ar: Seq[Double], ma: Seq[Double], omega: Double, alpha: Seq[Double], beta: Seq[Double]) {
    override def toString: String = {
      s"Parameters(mu = $mu, ar = (${ar.mkString(",")}), ma = (${ma.mkString(",")}), omega = $omega, alpha = (${alpha.mkString(",")}), beta = (${beta.mkString(",")}))"
    }
  }

  object Parameters {
    def apply(v: Seq[EstimatedValue]): Parameters = {
      val modelParameters = 1 + spec.mean.arOrder + spec.mean.maOrder + 1 + spec.innovations.alphaOrder + spec.innovations.betaOrder
      assume(
        v.length == modelParameters,
        s"Illegal model estimates length, expected = $modelParameters, got = ${v.length}"
      )
      val p = scala.collection.mutable.ListBuffer(v.map(_.value): _*)
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

  def apply(n: Int): Seq[Forecast] = {
    val Parameters(mu, ar, ma, omega, alpha, beta) = Parameters(estimate.estimates.map(_.estimate))

    def initialize() = {
      val n = estimate.data.length

      val x0 = estimate.data.slice(n-spec.mean.arOrder, n).activeValuesIterator.toSeq
      val err0 = estimate.err.slice(n-spec.mean.maOrder, n).activeValuesIterator.toSeq
      val errSq0 = estimate.err.slice(n-spec.innovations.alphaOrder, n).activeValuesIterator.toSeq.map(math.pow(_, 2))
      val sigmaSq0 = estimate.sigma.slice(n-spec.innovations.betaOrder, n).activeValuesIterator.toSeq.map(math.pow(_, 2))

      (x0, err0, errSq0, sigmaSq0)
    }

    // Initialize error & sigma with mean squared error
    val (x0, err0, errSq0, sigmaSq0) = initialize()

    // Initialize ARMA-GARCH recursion
    var _x = x0
    var _err = err0
    var _errSq = errSq0
    var _sigmaSq = sigmaSq0

    def multiply: (Double, Double) => Double = {
      case (d1, d2) => d1 * d2
    }

    val dataMean = breeze.linalg.mean(estimate.data)

    // Forecast sigma squared
    val sigmaSqForecast = for (i <- 0 until n) yield {
      val mean = mu + (ar zip _x).map(multiply.tupled).sum + (ma zip _err).map(multiply.tupled).sum
      val sigmaSq = omega + (alpha zip _errSq).map(multiply.tupled).sum + (beta zip _sigmaSq).map(multiply.tupled).sum

      // Update recursion data for next step
      if (!ar.isEmpty)    _x       = mean              +: _x.dropRight(1)
      if (!ma.isEmpty)    _err     = (mean - dataMean) +: _err.dropRight(1)
      if (!alpha.isEmpty) _errSq   = sigmaSq           +: _errSq.dropRight(1)
      if (!beta.isEmpty)  _sigmaSq = sigmaSq           +: _sigmaSq.dropRight(1)
      (mean, sigmaSq)
    }

    sigmaSqForecast.map {
      case (m, sigmaSq) => spec.Forecast(m, math.sqrt(math.abs(sigmaSq)))
    }
  }
}
