package common.las

import math._

abstract class Method(L: Array[Array[Double]], r: Array[Double]) {
  def matrixRepr(a: Array[Array[Double]]): String = {
    val s = StringBuilder.newBuilder
    for (row <- a) s ++= row map (el => f"$el%7.4f") mkString("(", " ", ")\n")
    s.result
  }

  def vectorRepr(v: Array[Double]): String = {
    val s = StringBuilder.newBuilder
    for (el <- v) s ++= "(" + f"$el%7.4f" + ")\n"
    s.result
  }

  def error(a: Array[Double], b: Array[Double]): Double = {
    val delta = (0 until dim).map( i => a(i) - b(i))
    abs(delta.maxBy(abs))
  }

  val A = L map (_.clone)
  val dim = A.length
  val f = r.clone

  val x = Array.ofDim[Double](dim)

  def X = println(vectorRepr(x))

  def lhs = println(matrixRepr(L))

  def rhs = println(vectorRepr(r))

}