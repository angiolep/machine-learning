package ml

import org.scalatest._

class LinearRegressionSpec extends FlatSpec with MustMatchers {
  "The LinearRegression" should "load data from file" in {
    import ml.LinearRegression._
    val file = classOf[LinearRegressionSpec].getResource("/data.csv").toURI.getPath
    val data = load(file)
    data must have size(97)
    data(0)(0) mustBe 6.1101
    data(96)(1) mustBe 0.61705
  }
}
