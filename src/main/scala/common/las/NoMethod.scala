package common.las

case object NoMethod extends Method(Array.empty[Array[Double]], Array.empty[Double]) {
  override val x = Array.ofDim[Double](1)
  x(0) = Double.NaN

  implicit def toIterative = new Iterative(Array.empty[Array[Double]], Array.empty[Double], 0, Array.empty[Double]) 

}
