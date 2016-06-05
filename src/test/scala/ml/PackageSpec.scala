package ml

import org.scalatest._

class PackageSpec extends FlatSpec with MustMatchers {
  "ml" should "calculate the dot product" in {
    val v1 = V(1.0, 2.0, 3.0)
    val v2 = V(4.0, 5.0, 6.0)
    v1 * v2 === 28
  }

  it should "calculate the transpose matrix" in {
    val m = M(
      V(1, 2),
      V(3, 4),
      V(5, 6)
    )
    m.tr must contain theSameElementsInOrderAs M(
      V(1, 3, 5),
      V(2, 4, 6)
    )
  }
}
