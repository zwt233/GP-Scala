package kernel

import breeze.linalg.{DenseMatrix => BDM, DenseVector => BDV}

/**
  * Covariance function given two points.
  */
trait Covariance {

  /**
    * the covariance function
    * @param x1
    * @param x2
    * @param params
    * @return
    */
  def cov(x1: BDM[Double],
          x2: BDM[Double],
          params: BDV[Double]): BDM[Double]

  /**
    * the derivative of covariance function against kernel hyper-parameters
    * @param x1
    * @param x2
    * @param params
    * @return
    */
  def grad(x1: BDM[Double],
           x2: BDM[Double],
           params: BDV[Double]): Array[BDM[Double]]

}
