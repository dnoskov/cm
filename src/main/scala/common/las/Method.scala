package common.las

import math._

abstract class Method(L: Array[Array[Double]], r: Array[Double]) {

  val fmt = "%10.6f"

  def matrixRepr(a: Array[Array[Double]]): String = {
    val s = StringBuilder.newBuilder

    for (row <- a) s ++= row map (el => fmt.format(el)) mkString("(", " ", ")\n")
    s.result
  }

  def vectorRepr(v: Array[Double]): String = {
    val s = StringBuilder.newBuilder
    for (el <- v) s ++= "(" + fmt.format(el) + ")\n"
    s.result
  }

  def rowRepr(v: Array[Double]): String = {
    v map { el => fmt.format(el) } mkString("(", " ", ")")
  }

  val A = L map (_.clone)
  val dim = A.length
  val f = r.clone

  val x = Array.ofDim[Double](dim)

  def X = rowRepr(x)

  def lhs = println(matrixRepr(L))

  def rhs = println(vectorRepr(r))

}
