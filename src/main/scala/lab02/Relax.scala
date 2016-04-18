package lab02

import common.las._

case class Relax(L: Array[Array[Double]], r: Array[Double],
  eps: Double,
  omega: Double,
  x0: Array[Double] = Array.empty[Double]) extends Iterative(L,r,eps,x0) {

  val debug = false

  def iterate: Unit = {
    (0 until dim).map( i => x0(i) = x1(i))
    for (i <- 0 until dim) {
      var s: Double = 0
      for (j <- 0 until i) s += A(i)(j) * x1(j)
      for (j <- i+1 until dim) s += A(i)(j) * x0(j)

      x1(i) = (omega/A(i)(i)) * (f(i) - s) + (1 - omega) * x0(i)

      iterations += 1

    }

  }


  // Начинаем итерации
  do {
    iterate
  }while(!endCondition)

  for (i <- 0 until dim)
    x(i) = x0(i)
}
