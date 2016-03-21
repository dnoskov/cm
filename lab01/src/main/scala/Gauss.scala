/**
  * Created by noname on 21.03.16.
  */
case class Gauss(a: Array[Array[Double]], f: Array[Double]) {
  val m = a.length

  //  Прямой ход

  val c = Array.ofDim[Double](m, m)
  val y = new Array[Double](m)

  for (k <- 0 until m) {
    for (j <- k+1 until m) c(k)(j) = a(k)(j) / a(k)(k)
    y(k) = f(k) / a(k)(k)
    for (i <- k+1 until m) {
      f(i) -= a(i)(k) * y(k)
      for (j <- k+1 until m) a(i)(j) -= a(i)(k) * c(k)(j)
    }
  }

  // Обратный ход

  val x = new Array[Double](m)
  x(m-1) = y(m-1)

  for (i <- m-2 to 0 by -1) x(i) = y(i) - (i+1 until m).map((j) => c(i)(j) * x(j)).sum

  def solution = x
  def resultlhs = c
  def resultrhs = y
}
