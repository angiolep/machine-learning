package ml

import org.scalatest._

class LinearAlgebraSpec extends FlatSpec with MustMatchers {
  "LinearAlgebra"  should "calculate the sum/product between two vectors" in {
    val v1 = V(1, 2, 3)
    val v2 = V(4, 5, 6)
    v1 + v2 must contain theSameElementsInOrderAs V(5, 7, 9)
    v1 - v2 must contain theSameElementsInOrderAs V(-3, -3, -3)
    v1 * v2 must contain theSameElementsInOrderAs V(4, 10, 18)
  } 
  
  it should "calculate the dot product between two vectors" in {
    val v1 = V(1, 2, 3)
    val v2 = V(4, 5, 6)
    v1.dotProd(v2) mustBe 32
  }

  it should "calculate the product between a matrix and a vector" in {
    val m = M(V(1, 2, 3), V(4, 5, 6))
    val v = V(10, 20, 30)
    m ** v must contain theSameElementsInOrderAs V(140, 320)
  }

  it should "calculate the transpose of a matrix" in {
    val m = M(V(1, 2), V(3, 4), V(5, 6))
    m.tr must contain theSameElementsInOrderAs M(V(1, 3, 5), V(2, 4, 6))
  }

  it should "calculate the product between two matrices" in {
    val m1 = M(V(1, 3, 2), V(4, 0, 1))
    val m2 = M(V(1, 3), V(0, 1), V(5, 2))
    m1 *** m2 must contain theSameElementsInOrderAs M(V(11, 10), V(9,14))
  }
}
