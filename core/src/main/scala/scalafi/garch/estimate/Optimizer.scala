package scalafi.garch.estimate

import breeze.linalg.DenseVector
import breeze.optimize.{ApproximateGradientFunction, LBFGS}
import org.apache.commons.math3.analysis.MultivariateFunction
import org.apache.commons.math3.optimization.GoalType
import org.apache.commons.math3.optimization.direct.CMAESOptimizer
import scala.language.implicitConversions

sealed trait Optimizer {
  def optimize(objectiveFunction: DenseVector[Double] => Double, 
               start: DenseVector[Double],
               lowerBound: Option[DenseVector[Double]],
               upperBound: Option[DenseVector[Double]]
                ): Either[EstimationError, DenseVector[Double]]

  private def boundsPenalty(params: DenseVector[Double], bounds: DenseVector[Double], penalize: (Double, Double) => Boolean): Double = {
    assume(params.length == bounds.length, s"Parameters length doesn't match bound length")
    
    val penalty = 10
    
    val penalties = (params.activeValuesIterator zip bounds.activeValuesIterator).map {
      case (param, bound) if penalize(param, bound) => penalty
      case _ => 0
    }
    
    penalties.sum    
  }

  protected def lowerBoundPenalty(params: DenseVector[Double], lowerBounds: DenseVector[Double]) = {
    boundsPenalty(params, lowerBounds, {
      case (param, bound) => param <= bound
    })                                         
  }

  protected def upperBoundPenalty(params: DenseVector[Double], upperBounds: DenseVector[Double]) = {
    boundsPenalty(params, upperBounds, {
      case (param, bound) => param >= bound
    })
  }
}

object Optimizer {

  object BreezeOptimizer extends Optimizer {
    private val optimizer = new LBFGS[DenseVector[Double]]()


    override def optimize(objectiveFunction: (DenseVector[Double]) => Double,
                          start: DenseVector[Double],
                          lowerBound: Option[DenseVector[Double]],
                          upperBound: Option[DenseVector[Double]]): Either[EstimationError, DenseVector[Double]] = {

      val f = new ApproximateGradientFunction((p: DenseVector[Double]) => {
        val objective = objectiveFunction(p)
        val lowerPenalty = lowerBound.map(b => lowerBoundPenalty(p, b)).getOrElse(0.0)
        val upperPenalty = upperBound.map(b => upperBoundPenalty(p, b)).getOrElse(0.0)
        objective + lowerPenalty + upperPenalty
      })
      val result = optimizer.minimize(f, start)

      Right(result)

    }
  }

  object ApacheMathOptimizer extends Optimizer {
    private val optimizer = new CMAESOptimizer()

    implicit def array2denseVector(arr: Array[Double]) = DenseVector(arr)

    implicit def denseVector2array(v: DenseVector[Double]) = v.toArray


    override def optimize(objectiveFunction: (DenseVector[Double]) => Double,
                          start: DenseVector[Double],
                          lowerBound: Option[DenseVector[Double]],
                          upperBound: Option[DenseVector[Double]]): Either[EstimationError, DenseVector[Double]] = {

      val f = new MultivariateFunction {
        override def value(point: Array[Double]): Double = objectiveFunction(point)
      }

      import scalaz.std.option._
      import scalaz.syntax.applicative._

      val bounded = ^(lowerBound, upperBound) {
        case (lower, upper) => optimizer.optimize(100, f, GoalType.MINIMIZE, start, lower, upper)
      }

      val result = bounded getOrElse optimizer.optimize(100, f, GoalType.MINIMIZE, start)

      Right(result.getPoint)
    }
  }
}
