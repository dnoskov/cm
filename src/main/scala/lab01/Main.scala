package lab01

import common.input.Octave._

object Main extends App {

  val (lhs, rhs) = readOctaveSave(f"octave/lab01/${args(1)}")

  def S(A: Array[Array[Double]], b: Array[Double]) = args(0) match {
    case "Gauss" => Gauss(A, b)
    case "GaussMaxRows" => GaussMaxRows(A, b)
    case "GaussMaxCols" => GaussMaxCols(A,b)
    case "SqRoot" => SqRoot(A, b)
    case "Triagonal" => Triagonal(A,b)
  }

  val result = S(lhs, rhs)

  println(f"x^T = ${result.X}")
}
