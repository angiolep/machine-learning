package ml

import org.scalatest._
import ml.LinearRegression._

class LinearRegressionSpec extends FlatSpec with MustMatchers {
  val file = classOf[LinearRegressionSpec].getResource("/data.csv").toURI.getPath
  val data = load(file)

  "The LinearRegression" should "load data from file" in {
    data must have size(97)
    data(0)(0) mustBe 6.1101
    data(96)(1) mustBe 0.61705
  }

  it should "calculate the cost function" in {
    val x = data.map(row => row(0))
    val y = data.map(row => row(1))
    val theta = V(0.0, 0.0)
    cost(x, y, theta) === (32.07 +- 0.01)
  }
}
