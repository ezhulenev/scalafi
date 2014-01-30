package scalafi.garch

import scalafi.garch.GarchModel.Garch
import breeze.linalg.DenseVector

sealed trait GarchFit[G <: GarchModel] {
  def model: G
}

object GarchFit {

  case class GarchFit(model: Garch, p: DenseVector[Double], q: DenseVector[Double]) extends scalafi.garch.GarchFit[Garch] {
    assume(p.length == model.p, s"Parameter 'p' (${model.p}) doesnt' match p-coefficient vector length '${p.length}'")
    assume(q.length == model.q, s"Parameter 'q' (${model.q}) doesn't match q-coefficient vector length '${q.length}'")
  }

}