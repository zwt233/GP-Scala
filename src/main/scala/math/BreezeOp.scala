package math

import breeze.linalg.{DenseMatrix => BDM, DenseVector => BDV}
import breeze.linalg.{cholesky, diag, inv, sum, trace}
import breeze.numerics.log

import scala.math.Pi

object BreezeOp {

  /**
    * calculate the inverse of a matrix with cholesky decomposition
    *
    * @param L : the Cholesky decomposition of matrix A where A = L'*L
    * @return inv(A)=inv(L)*inv(L')
    */
  def choleskyInv(L: BDM[Double]): BDM[Double] = {
    val invL = inv(L)
    invL * invL.t
  }

  /**
    * sum of log diag of positive definite matrices
    *
    * @param L
    * @return
    */
  def sumLogDiag(L: BDM[Double]): Double = {
    2 * sum(log(diag(L)))
  }

  def logLike(meanX: BDV[Double],
              KXX: BDM[Double],
              invKXX: BDM[Double],
              y: BDV[Double]): Double = {

    val m = meanX

    val logDiag = sumLogDiag(cholesky(KXX))

    val value = -0.5 * (y - m).t * invKXX * (y - m) - 0.5 * logDiag - 0.5 * meanX.size * scala.math.log(2 * Pi)

    value(0)
  }

  def logLikeD(meanX: BDV[Double],
               invKXX: BDM[Double],
               y: BDV[Double],
               covGrads: Array[BDM[Double]]): BDV[Double] = {

    val m = meanX
    val alpha = invKXX * (y - m)

    val grads = covGrads.map { covGrad =>
      val tmp = alpha * alpha.t - invKXX
      0.5 * trace(tmp * covGrad)
    }

    BDV(grads)
  }

  def cartesian(A:Array[Double], B:Array[Double]) = for (a <- A; b <- B) yield {
    Array(a,b)
  }

  def cartesian(A:Array[Array[Double]], B:Array[Double]) = for (a <- A; b <- B) yield {
    (a.toBuffer+=b).toArray
  }
}