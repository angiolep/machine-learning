package ml

import LinearRegression._

object LinearRegressionApp {
  def main(args: Array[String]): Unit = {
    val file = classOf[LinearRegressionSpec].getResource("/data.csv").toURI.getPath
    val data = load(file)
    val x = M(data.col(0))
    val y = data.col(1)
    //val theta = V(0.0, 0.0)
    gradientDescent(x, y, alpha = 0.01, 100)
  }
}
