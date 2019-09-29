package math

import breeze.generic.UFunc
import breeze.linalg.{DenseMatrix => BDM}
import breeze.linalg._

/**
  * Computes pair-wise square distances between matrices x1 and x2.
  *
  * @param x1 [N x D]
  * @param x2 [M x D]
  * @return matrix of square distances [N x M]
  */
object SquareDist extends UFunc {

  implicit object implBinary
    extends Impl2[BDM[Double], BDM[Double], BDM[Double]] {

    def apply(x1: BDM[Double],
              x2: BDM[Double]): BDM[Double] = {

      val t1 = -2.0 * (x1 * x2.t)

      val t2 = t1(*, ::) + sum(x2.t *:* x2.t, Axis._0).t

      t2(::, *) + sum(x1.t *:* x1.t, Axis._0).t
    }
  }
}