package scalafi.garch.estimate

import breeze.linalg.{DenseMatrix, DenseVector}

trait Hessian {
  private[this] val Step = 1e-4

  protected def hessian(f: DenseVector[Double] => Double, point: DenseVector[Double]): DenseMatrix[Double] = {
    val n = point.length

    val epsilon = point.copy *= Step

    val Hessian = DenseMatrix.zeros[Double](n, n)

    for (i <- 0 until n) {
      for (j <- 0 until n) {
        val x1 = point.copy
        val x2 = point.copy
        val x3 = point.copy
        val x4 = point.copy

        x1.update(i, x1(i) + epsilon(i)); x1.update(j, x1(j) + epsilon(j))
        x2.update(i, x2(i) + epsilon(i)); x2.update(j, x2(j) - epsilon(j))
        x3.update(i, x3(i) - epsilon(i)); x3.update(j, x3(j) + epsilon(j))
        x4.update(i, x4(i) - epsilon(i)); x4.update(j, x4(j) - epsilon(j))

        val v = (f(x1) - f(x2) - f(x3) + f(x4)) / (4*epsilon(i)*epsilon(j))
        Hessian.update(i ,j, v)
      }
    }

    Hessian
  }
}
