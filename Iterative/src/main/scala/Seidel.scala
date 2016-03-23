object Seidel extends App{

  def method(a:Array[Array[Double]],b:Array[Double]) ={
    val n = a.length
    val eps = 0.00001
    var p = Array.empty[Double]
    val x = new Array[Double](n)
    def converge(xk:Array[Double], xkp:Array[Double]):Boolean = {
      var norm:Double = 0
      for (i <- 0 until n)
        norm += (xk(i) - xkp(i))*(xk(i) - xkp(i))
      if(Math.sqrt(norm) >= eps) false
      else true
    }
    do {
      p = x.clone()
      for (i <- 0 until n) {
        var v:Double = 0
        for (j <- 0 until i)
          v += (a(i)(j) * x(j))
        for (j <- i+1 until n)
          v += (a(i)(j) * p(j))
        x(i) = (b(i) - v) / a(i)(i)
      }
    }
    while (!converge(x, p))
    x
  }
  def test1 = method(Array(Array(10,1,1),Array(2,10,1),Array(2,2,10)),Array(12,13,14))
  test1.foreach(x => print(x.toString+" "))
}
