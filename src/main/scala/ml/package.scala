package object ml {
  type V = List[Double]
  type M = List[V]

  object V {
    def apply(d: Double*) = List(d: _*)
  }

  object M {
    def apply(v: V*) = List(v: _*)
  }

  /**
   * The dot product takes two equal-length sequences of numbers (Vs) and
   * returns a single number calculated as the sum of the products of the
   * corresponding entries of the two given sequences.
   *
   */
  def dotProd(v1: V, v2: V): Double =
    (0.0 /: v1.zip(v2).map(t => t._1 * t._2))(_ + _)

  implicit class RichV(v: V) {
    def * (v2: V): Double = dotProd(v, v2)
  }


  /*
   * The transpose of a matrix A is another matrix A'  created by any one
   * of the following equivalent actions:
   *
   *  - reflect A over its main diagonal (which runs from
   *    top-left to bottom-right) to obtain AT
   *  - write the rows of A as the columns of A'
   *  - write the columns of A as the rows of A'
   *
   *    1  2             1  3  5
   *    3  4     -->     2  4  6
   *    5  6
   */
  def transpose(m: M): M = {
    if (m.head.isEmpty) Nil
    else m.map(_.head) :: transpose(m.map(_.tail))
  }

  implicit class RichMatrix(m: M) {
    def tr: M = transpose(m)
  }
}
