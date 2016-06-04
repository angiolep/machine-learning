package ml

import scala.io.Source
import java.net.URI

object LinearRegression {

  type V = List[Double]
  type M = List[V]


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
