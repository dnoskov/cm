package lab02

import common.input.Octave._
import common.las._

object Main extends App {
  val (lhs, rhs) = readOctaveSave(f"octave/${args(1)}")

  def S(A: Array[Array[Double]], b: Array[Double]) = args(0) match {

    case "Jacobi" => Jacobi(lhs, rhs, args(2).toDouble,
      if (args.length >3)
        (args.slice(3, 3+lhs.length).map { s => s.toDouble}).toArray
      else
        Array.ofDim[Double](rhs.length)
    )

    case "Zeidel" => Zeidel(lhs, rhs, args(2).toDouble,
      if (args.length > 3)
        (args.slice(3, 3+lhs.length).map { s => s.toDouble}).toArray
      else
        Array.ofDim[Double](rhs.length)
    )

    case "Relax" => Relax(lhs, rhs, args(2).toDouble, args(3).toDouble,
      if (args.length > 4)
        (args.slice(4, 4+lhs.length).map { s => s.toDouble}).toArray
      else
        Array.ofDim[Double](rhs.length)
    )

    case _ => NoMethod.toIterative
  }

  val result = S(lhs, rhs)
  println(f"x^T = ${result.X}")
  println(f"итераций: ${result.iterations}")
}
