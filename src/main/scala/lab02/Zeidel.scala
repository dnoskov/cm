package lab02

import common.las.Method

case class Zeidel(L: Array[Array[Double]], r: Array[Double],
  eps: Double,
  var x0: Array[Double]) extends Method(L,r) {

  if (x0.length == 0) x0 = Array.ofDim[Double](dim)

  val x1 = Array.ofDim[Double](dim)


  // Находим первое приближение
  for (i <- 0 until dim) {
    x1(i) = (f(i)
      - (0 until i).map( j => A(i)(j) * x1(j)).sum
      - (i+1 until dim).map( j => A(i)(j) * x0(j)).sum) / A(i)(i)
  }


  // Начинаем итерации 
  k = 1 // начальное значение есть => одна итерация выполнена
  while(error(x0, x1) >= eps) {
    (0 until dim).map( i => x0(i) = x1(i))
     for (i <- 0 until dim) {
       x1(i) = (f(i)
         - (0 until i).map( j => A(i)(j) * x1(j)).sum
         - (i+1 until dim).map( j => A(i)(j) * x0(j)).sum) / A(i)(i)
    }
    k += 1
  }

  for (i <- 0 until dim)
    x(i) = x0(i)
}
