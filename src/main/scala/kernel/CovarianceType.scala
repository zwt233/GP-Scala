package kernel

object CovarianceType extends Enumeration {

  type CovarianceType = Value

  val MATERN3 = Value("MATERN3")
  val MATERN5 = Value("MATERN5")
  val MATERN5_ISO = Value("MATERN5_ISO")
  val SQUAREEXP_ISO = Value("SQUAREEXP_ISO")

  def parse(name: String): Covariance = {
    val covType = CovarianceType.withName(name.toUpperCase())
    parse(covType)
  }

  def parse(covType: CovarianceType.Value): Covariance = covType match {
    case MATERN3 => new Matern3
    case MATERN5 => new Matern5
    case MATERN5_ISO => new Matern5Iso
    case SQUAREEXP_ISO => new SquareExpIso
    case _ => new Matern5
  }

}
