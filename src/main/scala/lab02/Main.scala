package lab02

import common.input.Octave._
import common.las._

object Main extends App {
  val (lhs, rhs) = readOctaveSave(f"octave/lab02/${args(1)}")

  def S(A: Array[Array[Double]], b: Array[Double]) = args(0) match {

    case "Jacobi" => Jacobi(lhs, rhs, args(2).toDouble,
      if (args.length >3)
        (args.slice(3, 3+lhs.length-1).map { s => s.toDouble}).toArray
      else
        Array.empty[Double]
    )

    case "Zeidel" => Zeidel(lhs, rhs, args(2).toDouble,
      if (args.length > 3)
        (args.slice(3, 3+lhs.length-1).map { s => s.toDouble}).toArray
      else
        Array.empty[Double]
    )

    case _ => NoMethod

  }

  val result = S(lhs, rhs)
  println(f"x^T = ${result.X}")
  println(f"итераций: ${result.k}")

}
