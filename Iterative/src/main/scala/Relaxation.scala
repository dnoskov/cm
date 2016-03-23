object Relaxation extends App{

  def method(a:Array[Array[Double]],b:Array[Double]) ={
    var omg = 1.0
    val n = a.length
    val eps = 0.00001
    var p = Array.empty[Double]
    var x = new Array[Double](n)
    def initState() = {x = (0 until n map(i => b(i)/a(i)(i))).toArray}
    def evalOmg:Double =
      (for(w <- 1 to 20) yield ({
          omg = w/10
          initState()
          (1 to 10) foreach (_ => interation())
          (for((x1,x2) <- x zip p) yield Math.abs(x1-x2)).max
        },w)).min._2
    def converge(xk:Array[Double], xkp:Array[Double]):Boolean = {
      val norm = (for (i <- 0 until n)yield (xk(i) - xkp(i))*(xk(i) - xkp(i))).sum
      if(Math.sqrt(norm) >= eps) false
      else true
    }
    def interation():Unit = {
      p = x.clone()
      for (i <- 0 until n) {
        var v:Double = 0
        for (j <- 0 until i)
          v += a(i)(j) * x(j)
        for (j <- i+1 until n)
          v += a(i)(j) * p(j)
        x(i) = omg * (b(i) - v) / a(i)(i) - p(i)*(omg-1)
      }
    }
    omg = evalOmg
    do {
      interation()
    }
    while (!converge(x, p))
    x
  }
  def test1 = method(Array(Array(10,1,1),Array(2,10,1),Array(2,2,10)),Array(12,13,14))
  test1.foreach(x => print(x.toString+" "))
}
