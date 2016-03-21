/**
  * Created by noname on 21.03.16.
  */
case class GaussMax(a: Array[Array[Double]], f: Array[Double]) {
  val m = a.length

  def swaprows(i: Int, j: Int) = {
    val tmpa = a(i)
    val tmpf = f(i)
    a(i) = a(j)
    a(j) = tmpa
    f(i) = f(j)
    f(j) = tmpf
  }

  def maxElRowIndex(col: Int): Int = {
    var maxi = 0
    for (i <- 1 until m) maxi = if (a(maxi)(col) < a(i)(col)) i else maxi
    maxi
  }

  //  Прямой ход

  val c = Array.ofDim[Double](m, m)
  val y = new Array[Double](m)

  for (k <- 0 until m) {
    if (a(k)(k) == 0) swaprows(k, maxElRowIndex(k))
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
