package lab02

import common.las.Method

case class Jacobi(L: Array[Array[Double]], r: Array[Double],
  eps: Double,
  x0: Array[Double]) extends Method(L,r) {

  val x1 = Array.ofDim[Double](dim)
  for (i <- 0 until dim) {
    x1(i) = (f(i) - (0 until i).map( j => A(i)(j) * x0(j)).sum - (i+1 until dim).map( j => A(i)(j) * x0(j)).sum)/A(i)(i)
  }

  var k = 0

  while(error(x0, x1) >= eps) {
    (0 until dim).map( i => x0(i) = x1(i))
     for (i <- 0 until dim) {
      x1(i) = (f(i) - (0 until i).map( j => A(i)(j) * x0(j)).sum - (i+1 until dim).map( j => A(i)(j) * x0(j)).sum)/A(i)(i)
    }
    k += 1
  }

  for (i <- 0 until dim)
    x(i) = x0(i)
}