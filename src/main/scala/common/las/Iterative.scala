package common.las

import math._

class Iterative(L: Array[Array[Double]], r: Array[Double],
  eps: Double,
  x0: Array[Double] = Array.empty[Double]) extends Method(L,r) {

  val x1 = if (x0.isEmpty) Array.empty[Double] else Array.ofDim[Double](dim)

  val initialDeltaNorm = norm(delta)

  var iterations = 0

  def delta: Array[Double] = x0 zip x1 map { case (a,b) => a-b }

  def norm(a: Array[Double]): Double = a.map ( a => abs(a) ).sum

  def endCondition: Boolean = norm(delta) <= eps

}
