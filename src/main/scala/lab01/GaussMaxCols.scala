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
    A(row).indexOf(A(row).maxBy(e => abs(e)))
  }

  //  Прямой ход
  val c = Array.ofDim[Double](dim, dim)
  val y = Array.ofDim[Double](dim)

  def C = println(matrixRepr(c))

  def Y = println(vectorRepr(y))

  for (k <- 0 until dim) {
    if (A(k)(k) == 0) swapcols(k, maxElColIndex(k))
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
