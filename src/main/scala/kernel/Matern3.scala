package kernel

import breeze.linalg.{DenseMatrix => BDM, DenseVector => BDV}
import breeze.linalg._
import breeze.numerics.{exp, pow, sqrt}
import math.SquareDist

/**
  * Matern covariance function with v = 3/2
  * (1 + sqrt(3)*r/l) * exp(-sqrt(3)*r/l)
  * Here r is the distance |x1-x2| of two points
  * Hyper-parameter: l is the length scale
  */
case class Matern3() extends Covariance {

  /**
    * the covariance function
    *
    * @param x1
    * @param x2
    * @param params
    * @return
    */
  override def cov(x1: BDM[Double],
                   x2: BDM[Double],
                   params: BDV[Double]): BDM[Double] = {

    require(params.size == 1,
      s"Number of hyper parameters is ${params.length} while expected 1")

    val l = params(0)

    val distMat = SquareDist(x1, x2)
    val r = sqrt(distMat)

    val vPart = sqrt(3) * r / l + 1.0
    val expPart = exp( -sqrt(3) * r / l )
    val covMatrix = vPart *:* expPart

    covMatrix
  }

  /**
    * the derivative of covariance function against kernel hyper-parameters
    *
    * @param x1
    * @param x2
    * @param params
    * @return
    */
  override def grad(x1: BDM[Double],
                    x2: BDM[Double],
                    params: BDV[Double]): Array[BDM[Double]] = {

    require(params.size == 1,
      s"Number of hyper parameters is ${params.length} while expected 1")

    val l = params(0)

    val distMat = SquareDist(x1, x2)
    val r = sqrt(distMat)

    val vPart = sqrt(3) * r / l + 1.0
    val expPart = exp( -sqrt(3) * r / l )

    val vPartGrad = -( sqrt(3) * r / pow(l, 2) ) *:* expPart
    val expPartGrad = vPart *:* expPart *:* ( sqrt(3) * r / pow(l, 2) )

    val gradL = vPartGrad + expPartGrad

    Array(gradL)
  }
}

