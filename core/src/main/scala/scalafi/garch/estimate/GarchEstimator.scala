package scalafi.garch.estimate

import scalafi.garch.GarchModel.Garch

class GarchEstimator(model: Garch, data: Seq[Double]) extends MaximumLikelihoodEstimator {

  import breeze.stats.DescriptiveStats.{mean => sampleMean}

  case class Parameters(mu: Double, omega: Double, alpha: Double, beta: Double)

  private[this] lazy val unitDistribution = breeze.stats.distributions.Gaussian(mu = 0, sigma = 1)

  private[this] def density(z: Double, hh: Double) = unitDistribution.pdf(z / hh) / hh

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

}