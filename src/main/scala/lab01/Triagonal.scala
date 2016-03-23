package lab01

import common.las.Method

case class Triagonal(L: Array[Array[Double]], r: Array[Double]) extends Method(L, r) {
  val a = Array.ofDim[Double](dim)
  val b = Array.ofDim[Double](dim)
  val c = Array.ofDim[Double](dim)
  val alpha = Array.ofDim[Double](dim - 1)
  val beta = Array.ofDim[Double](dim - 1)

  // Заполнение массивов
  for (i <- 0 until dim) {
    a(i) = if (i > 0) -A(i)(i - 1) else 0
    c(i) = A(i)(i)
    b(i) = if (i < dim - 1) -A(i)(i + 1) else 0
    if (i < dim - 1) {
      alpha(i) = b(i) / (c(i) - (if (i == 0) 0 else a(i) * alpha(i - 1)))
      beta(i) = (f(i) + (if (i == 0) 0 else a(i) * beta(i - 1))) / (c(i) - (if (i == 0) 0 else a(i) * alpha(i - 1)))
    }
  }

  x(dim - 1) = (f.last + a.last * beta.last) / (c.last - a.last * alpha.last)
  for (i <- dim - 2 to 0 by -1) x(i) = alpha(i) * x(i + 1) + beta(i)

}
