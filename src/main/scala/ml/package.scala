package object ml {

  implicit class RichDouble(double: Double) {
    def ^(exponent: Double): Double = scala.math.pow(double, exponent)
  }

  type V = List[Double]
  object V {
    def apply(d: Double*) = List(d: _*)
  }

  implicit class RichVector(v1: V) {
    private def zip(v1: V, v2: V, fn: (Double, Double) => Double) = v1.zip(v2).map(t => fn(t._1, t._2))

    def +(v2: V): V = zip(v1, v2, _ + _ )
    def -(v2: V): V = zip(v1, v2, _ - _ )
    // TODO def *(s: Double): V = v1.map(_ * s)
    def *(v2: V): Double = (0.0 /: zip(v1, v2, _ * _ ))(_ + _)
    def ^(e: Double): V = v1.map(_ ^ e)
  }


  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  type M = List[V]
  object M {
    def apply(v: V*) = List(v: _*)
  }

  implicit class RichMatrix(m1: M) {

    def |:(d: Double): M = List.fill(m1.size)(d) :: m1

    /**
      * Multiply this matrix by the supplied scalar value
      *
      * @param s is the scala value
      * @return a matrix
      */
    def *(s: Double): M = m1.map(row => row.map(cell => cell * s))

    /**
      * Multiply this matrix by the supplied column vector
      *
      * @param v is the column vector
      * @return a column vector
      */
    def **(v: V): V = m1.map(row => row * v)

    /**
      * Multiply this matrix by the supplied matrix
      *
      * @param m2
      * @return a matrix
      */
    def ***(m2: M): M = {
      for (m1row <- m1) yield {
        for (m2col <- m2.tr) yield {
          m1row * m2col
        }
      }
    }

    def cols: Int = m1.map(r => r.size).max

    def rows: Int = m1.size

    def col(c: Int): V = m1.map(r => r(c))

    def row(r: Int): V = m1(r)

    /**
      * @return the transpose
      */
    def tr: M = {
      def transpose(m: M): M = {
        if (m.head.isEmpty) Nil
        else m.map(_.head) :: transpose(m.map(_.tail))
      }
      transpose(m1)
    }
  }
}
