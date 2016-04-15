package lab01

import common.las.Method
import math._

case class GaussMaxCols(L: Array[Array[Double]], r: Array[Double]) extends Method(L, r) {

  def swapcols(i: Int, j: Int) = {
    for (k <- 0 until dim) {
      val tmp = A(i)(k)
      A(i)(k) = A(j)(k)
      A(j)(k) = tmp
    }
  }

  def maxElColIndex(row: Int): Int = {
    val sub = A(row).slice(row, dim-1)
    sub.indexOf(sub.maxBy(e => abs(e)))
  }

  //  Прямой ход
  val c = Array.ofDim[Double](dim, dim)
  val y = Array.ofDim[Double](dim)

  def C = println(matrixRepr(c))

  def Y = println(vectorRepr(y))

  for (k <- 0 until dim) {
    if (A(k)(k) == 0) swapcols(k, maxElColIndex(k))
    for (j <- k + 1 until dim) A(j)(k) = A(j)(k) / A(j)(j)
    y(k) = f(k) / A(k)(k)
    for (i <- k + 1 until dim) {
      f(k) -= A(k)(i) * y(i)
      for (j <- k + 1 until dim) A(j)(i) -= A(k)(j) * c(j)(k)
    }
  }

  // Обратный ход
  for (i <- dim - 1 to 0 by -1) x(i) = y(i) - (i + 1 until dim).map((j) => c(j)(i) * x(i)).sum

}
