import scala.collection.mutable.ArrayBuffer
import scala.io.Source

/**
  * Created by noname on 20.03.16.
  */
object Main extends App {

  def readOctaveSave(filename: String): (Array[Array[Double]], Array[Double]) = {
    val (lhsIterator, rhsIterator) = Source.fromFile(filename).getLines() span { (s) => s.nonEmpty }

    val lhs = ArrayBuffer.empty[Array[Double]]
    for (line <- lhsIterator if line.nonEmpty && !line.startsWith("#")) {
      val row = for (token <- line.split("\\s+") if token.nonEmpty) yield token.toDouble
      lhs += row
    }

    val rhs = ArrayBuffer.empty[Double]
    for (line <- rhsIterator if line.nonEmpty && !line.startsWith("#")) {
      for (token <- line.split("\\s+") if token.nonEmpty) rhs += token.toDouble
    }

    (lhs.toArray, rhs.toArray)
  }


}
