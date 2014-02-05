package scalafi.garch.estimate

import breeze.linalg.DenseVector
import scalafi.garch.estimate.Optimizer.BreezeOptimizer
import scalafi.garch._
import scala.collection.breakOut
import scalafi.garch.Spec
import org.slf4j.LoggerFactory
import scala.language.implicitConversions
import scala.annotation.implicitNotFound

@implicitNotFound(msg = "Model parameters type class not found for ${M}")
sealed trait ModelParameters[M <: Model] {
  def start(model: M, data: DenseVector[Double]): DenseVector[Double]

  def lowerBounds(model: M, data: DenseVector[Double]): DenseVector[Double]

  def upperBounds(model: M, data: DenseVector[Double]): DenseVector[Double]

  def estimate(model: M, estimates: Seq[EstimatedValue]): (M#Estimate, Seq[EstimatedValue])
}

object ModelParameters {
  import breeze.linalg.mean
  import breeze.linalg.variance

  implicit object ConstantMeanParameters extends ModelParameters[ConstantMean] {
    override def start(model: ConstantMean, data: DenseVector[Double]) = DenseVector(mean(data))

    override def lowerBounds(model: ConstantMean, data: DenseVector[Double]) = DenseVector(-10)

    override def upperBounds(model: ConstantMean, data: DenseVector[Double]) = DenseVector(10)

    override def estimate(model: ConstantMean, estimates: Seq[EstimatedValue]) = {
      (model.Estimate(estimates.head), estimates.tail)
    }
  }

  implicit object ArmaParameters extends ModelParameters[Arma] {
    override def start(model: Arma, data: DenseVector[Double]) = {
      val mu = mean(data)
      val ar = Seq.fill(model.arOrder)(0.0)
      val ma = Seq.fill(model.maOrder)(0.0)
      DenseVector(mu +: ar ++: ma :_*)
    }

    override def lowerBounds(model: Arma, data: DenseVector[Double]) = {
      val mu = -10.0
      val ar = Seq.fill(model.arOrder)(-1.0)
      val ma = Seq.fill(model.maOrder)(-1.0)
      DenseVector(mu +: ar ++: ma :_*)
    }

    override def upperBounds(model: Arma, data: DenseVector[Double]) = {
      val mu = -10.0
      val ar = Seq.fill(model.arOrder)(1.0)
      val ma = Seq.fill(model.maOrder)(1.0)
      DenseVector(mu +: ar ++: ma :_*)
    }

    override def estimate(model: Arma, estimates: Seq[EstimatedValue]) = {
      val p = scala.collection.mutable.ListBuffer(estimates:_*)
      def take(n: Int) = (0 until n).map(_ => p.remove(0)).toSeq
      (model.Estimate(take(1).head, take(model.arOrder), take(model.maOrder)), p.toSeq)
    }
  }

  implicit object GarchParameters extends ModelParameters[Garch] {
    private val bound = 1e-6

    override def start(model: Garch, data: DenseVector[Double]): DenseVector[Double] = model match {
      case Garch(1, 1) =>
        val omega = 0.1 * variance(data)
        val alpha = 0.2
        val beta = 0.5
        DenseVector(omega, alpha, beta)
      case _ => sys.error(s"Start parameters for '$model' are not supported")
    }

    override def lowerBounds(model: Garch, data: DenseVector[Double]): DenseVector[Double] = {
      val omega = bound * bound
      val alpha = Seq.fill(model.alphaOrder)(bound)
      val beta = Seq.fill(model.betaOrder)(bound)
      DenseVector(omega +: alpha ++: beta :_*)
    }

    override def upperBounds(model: Garch, data: DenseVector[Double]): DenseVector[Double] = {
      val omega = 100 * variance(data)
      val alpha = Seq.fill(model.alphaOrder)(1 - bound)
      val beta = Seq.fill(model.betaOrder)(1 - bound)
      DenseVector(omega +: alpha ++: beta :_*)
    }

    override def estimate(model: Garch, estimates: Seq[EstimatedValue]) = {
      val p = scala.collection.mutable.ListBuffer(estimates:_*)
      def take(n: Int) = (0 until n).map(_ => p.remove(0)).toSeq
      (model.Estimate(take(1).head, take(model.alphaOrder), take(model.betaOrder)), p.toSeq)
    }
  }

}

class Estimator[M <: Mean, I <: Innovations](data: DenseVector[Double], spec: Spec[M, I])
                                            (implicit mean: ModelParameters[M], innovations: ModelParameters[I]) extends Hessian {

  private val log = LoggerFactory.getLogger(this.getClass)
  private val likelihood = new ArmaGarchLikelihood(data, spec)

  type Estimate = Spec[M, I]#Estimate

  def estimate(optimizer: Optimizer = BreezeOptimizer): Either[EstimationError, Estimate] = {
    
    val start = DenseVector.vertcat(mean.start(spec.mean, data), innovations.start(spec.innovations, data))
    log.debug(s"Start point = $start, likelihood = ${likelihood(start)}")
    
    val lowerBound = DenseVector.vertcat(mean.lowerBounds(spec.mean, data), innovations.lowerBounds(spec.innovations, data))
    val upperBound = DenseVector.vertcat(mean.upperBounds(spec.mean, data), innovations.upperBounds(spec.innovations, data))

    def objectiveFunction(params: DenseVector[Double]) = likelihood(params)

    val result = optimizer.optimize(objectiveFunction, start, Some(lowerBound), Some(upperBound))

    result.right.map {
      case output =>
        import breeze.linalg.{diag, inv}
        import breeze.numerics.sqrt

        val end = output
        log.debug(s"End point = $end, likelihood = ${likelihood(end)}")

        val H = hessian(objectiveFunction, output)

        val stdErrors = sqrt(diag(inv(H)))
        val tValues = output.copy /= stdErrors

        val v = output
        val se = stdErrors
        val t = tValues

        val estimates: Seq[EstimatedValue] = (0 until v.length).map(i => EstimatedValue(v(i), se(i), t(i)))(breakOut)
        
        estimates.foreach(println)

        val (meanEstimate, meanLeftover) = mean.estimate(spec.mean, estimates)
        val (innovationsEstimate, innovationsLeftover) = innovations.estimate(spec.innovations, meanLeftover)

        assume(innovationsLeftover.isEmpty, s"Not all estimated values matched to estimate parameters: '$innovationsLeftover'")
      
        spec.Estimate(
          meanEstimate, innovationsEstimate,
          data, likelihood.err(end), likelihood.sigma(end)
        )
    }
  }
}