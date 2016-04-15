package lab02

import common.las._

case class Relax(L: Array[Array[Double]], r: Array[Double],
  eps: Double,
  omega: Double,
  x0: Array[Double] = Array.empty[Double]) extends Iterative(L,r,eps,x0) {


  val xtmp = x0.clone

  println("x0 = " + rowRepr(x0))
  // Находим первое приближение
  for (i <- 0 until dim) {
    println(f"     x1($i) = ")
    println(f"          $omega/A($i)($i) = ${omega/A(i)(i)} * ")
    println(f"          ( f($i) = " + f(i) + " - ")
    println(f"          (0 until $i).map( j => A($i)(j) * x1(j)).sum = " + (0 until i).map( j => A(i)(j) * x1(j)).sum + " - ")
    println(f"          (${i+1} until $dim).map( j => A(i)(j) * x0(j)).sum = " + (i+1 until dim).map( j => A(i)(j) * x0(j)).sum + ") + ")
    println(f"          (1 - $omega) * x0($i) = " + (1 - omega) * x0(i))

    x1(i) = (omega/A(i)(i)) * (f(i)
      - (0 until i).map( j => A(i)(j) * x1(j)).sum
      - (i+1 until dim).map( j => A(i)(j) * x0(j)).sum)
      + (1 - omega) * x0(i)
  }
  println("x1 = " + rowRepr(x1))
  println


  // Начинаем итерации
  iterations += 1
  while(!endCondition) {
    (0 until dim).map( i => x0(i) = x1(i))

    println("x0 = " + rowRepr(x0))

    for (i <- 0 until dim) {

      println(f"     x1($i) = ")
      println(f"          $omega/A($i)($i) = ${omega/A(i)(i)} * ")
      println(f"          ( f($i) = " + f(i) + " - ")
      println(f"          (0 until $i).map( j => A($i)(j) * x1(j)).sum = " + (0 until i).map( j => A(i)(j) * x1(j)).sum + " - ")
      println(f"          (${i+1} until $dim).map( j => A(i)(j) * x0(j)).sum = " + (i+1 until dim).map( j => A(i)(j) * x0(j)).sum + ") + ")
      println(f"          (1 - $omega) * x0($i) = " + (1 - omega) * x0(i))

      x1(i) = (omega/A(i)(i)) * (f(i)
        - (0 until i).map( j => A(i)(j) * x1(j)).sum
        - (i+1 until dim).map( j => A(i)(j) * x0(j)).sum)
        + (1 - omega) * x0(i)
    }
    println("x1 = " + rowRepr(x1))
    println

    iterations += 1
  }

  for (i <- 0 until dim)
    x(i) = x0(i)
}
