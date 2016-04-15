package lab02

import common.las._

case class Jacobi(L: Array[Array[Double]], r: Array[Double],
  eps: Double,
  x0: Array[Double] = Array.empty[Double] ) extends Iterative(L,r,eps,x0) {

  for (i <- 0 until dim) {
    x1(i) = (f(i) - (0 until i).map( j => A(i)(j) * x0(j)).sum - (i+1 until dim).map( j => A(i)(j) * x0(j)).sum)/A(i)(i)
  }

  iterations += 1

  while(!endCondition) {
    (0 until dim).map( i => x0(i) = x1(i))
     for (i <- 0 until dim) {
      x1(i) = (f(i) - (0 until i).map( j => A(i)(j) * x0(j)).sum - (i+1 until dim).map( j => A(i)(j) * x0(j)).sum)/A(i)(i)
    }
    iterations += 1
  }

  for (i <- 0 until dim)
    x(i) = x0(i)
}
