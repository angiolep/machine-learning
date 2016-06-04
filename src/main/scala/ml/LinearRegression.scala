package ml

import scala.io.Source
import java.net.URI

object LinearRegression {

  implicit class RichDouble(double: Double) {
    def ^(exponent: Double): Double = scala.math.pow(double, exponent)
  }

  type V = List[Double]
  object V {
    def apply(d: Double*) = List(d: _*)
  }
  
  type M = List[V]

  def h(x: Double, theta: V): Double = theta(0) + x * theta(1)

  def cost(x: V, y: V, theta: V) = {
    val m = x.size // y.size
    val rss = x.zip(y).map { case (x, y) => (h(x, theta) - y) ^ 2 }
    rss.sum / (2 * m)
  }

  def load(file: String): M = {
    val source = Source.fromFile(file)
    // IMPORTANT: the toList invocation is meant to fetch ALL the lines into memory
    //            and it prevents lazy streams to kick in
    val lines = source.getLines.toList
    val rows = lines.map(_.split(",").toList.map(_.toDouble))
    source.close()
    rows
  }
}
