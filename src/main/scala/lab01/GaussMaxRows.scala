package lab01

import common.las.Method

case class GaussMaxRows(L: Array[Array[Double]], r: Array[Double]) extends Method(L, r) {

  def swaprows(i: Int, j: Int) = {
    val tmpa = A(i)
    val tmpf = f(i)
    A(i) = A(j)
    A(j) = tmpa
    f(i) = f(j)
    f(j) = tmpf
  }

  def maxElRowIndex(col: Int): Int = {
    var maxi = col
    for (i <- col+1 until dim) maxi = if (A(maxi)(col) < A(i)(col)) i else maxi
    maxi
  }

  //  Прямой ход
  val c = Array.ofDim[Double](dim, dim)
  val y = Array.ofDim[Double](dim)

  def C = println(matrixRepr(c))

  def Y = println(vectorRepr(y))

  for (k <- 0 until dim) {
    if (A(k)(k) == 0) swaprows(k, maxElRowIndex(k))
    for (j <- k + 1 until dim) c(k)(j) = A(k)(j) / A(k)(k)
    y(k) = f(k) / A(k)(k)
    for (i <- k + 1 until dim) {
      f(i) -= A(i)(k) * y(k)
      for (j <- k + 1 until dim) A(i)(j) -= A(i)(k) * c(k)(j)
    }
  }

  // Обратный ход
  for (i <- dim - 1 to 0 by -1) x(i) = y(i) - (i + 1 until dim).map((j) => c(i)(j) * x(j)).sum

}
