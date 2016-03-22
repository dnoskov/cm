import math._

case class SqRoot(L: Array[Array[Double]], r: Array[Double]) extends Method(L, r) {

  val d = Array.ofDim[Double](dim, dim)
  val s = Array.ofDim[Double](dim, dim)
  val std = Array.ofDim[Double](dim, dim)
  val y = Array.ofDim[Double](dim)

  def D = println(matrixRepr(d))

  def S = println(matrixRepr(s))

  def StD = println(matrixRepr(std))

  def Y = println(vectorRepr(y))

  for (i <- 0 until dim; j <- i until dim) {
    if (i == 0 && j > 0) s(0)(j) = A(0)(j) / s(0)(0) / d(0)(0)
    else if (i == j) {
      val sum = (0 until i).map(k => pow(s(k)(i), 2) * d(k)(k)).sum
      d(i)(i) = signum(A(i)(i) - sum)
      s(i)(i) = sqrt(abs(A(i)(i) - sum))
    }
    else s(i)(j) = (A(i)(j) - (0 until i).map(k => s(k)(i) * d(k)(k) * s(k)(j)).sum) / s(i)(i) / d(i)(i)
  }

  //  Решение уравнения StD * y = f
  for (i <- 0 until dim) y(i) = (f(i) - (0 until i).map(k => s(k)(i) * d(k)(k) * y(k)).sum) / s(i)(i) / d(i)(i)

  // Решение уравнения S * x = y
  for (i <- dim - 1 to 0 by -1) x(i) = (y(i) - (dim - 1 to i by -1).map(k => s(i)(k) * x(k)).sum) / s(i)(i)

}
