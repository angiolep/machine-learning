package ml

import scala.io.Source


object LinearRegression {

  /**
    * Calculate the linear prediction vector
    *
    * @param x is the matrix of input features
    * @param theta is the vector of linear prediction parameters
    * @return the prediction vector
    */
  def h(x: M, theta: V) = (1.0 |: x) ** theta

  /**
    * Calculate the cost of prediction as half mean of squared errors for
    * the given training examples and linear prediction parameters
    *
    * ```
    * ((h(1.0 |: x, theta) - y) ^ 2).sum / (2 * (x.size min y.size))
    * ```
    *
    * @param x is the matrix of input training features
    * @param y is the vector of output training results
    * @param theta is the vector of linear prediction parameters
    * @return the cost of prediction for the given training examples and linear prediction parameters
    */
  def cost(x: M, y: V, theta: V): Double = {
    
    // calculate the sum of squared errors of predictions
    val sse: Double = ((h(x, theta) - y) ^ 2).sum

    // and finally their mean
    sse / (2 * (x.size min y.size))
  }


  def gradientDescent(x: M, y: V, alpha: Double, iterations: Int): V = {
    val m = x.size min y.size
    def slope(theta: V, j: Int) : Double = ((h(x, theta) - y) * x.col(j)).sum / m
    var theta = V(0.0, 0.0)
    for (k <- 1 to iterations) {
      println(cost(x, y, theta))
      theta = for((t,j) <- theta.zipWithIndex) yield {
        t - alpha * slope(theta, j)
      }
    }
    theta
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
