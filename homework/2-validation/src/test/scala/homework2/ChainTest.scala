package homework2

import org.scalatest.{FlatSpec, Matchers}

class ChainTest extends FlatSpec with Matchers {
  "++" should "append two chains" in {
    (Chain(1, 2) ++ Chain(3, 4)) shouldEqual Chain(1, 2, 3, 4)
  }

  "+:" should "prepend element" in {
    Chain(1, 2, 3).+:(0) shouldEqual Chain(0, 1, 2, 3)
  }

  ":+" should "append element" in {
    Chain(1, 2, 3).:+(0) shouldEqual Chain(1, 2, 3, 0)
  }

  "isEmpty" should "return false" in {
    Singleton(1).isEmpty shouldEqual false
    Append(Chain(1, 2, 3), Chain(2, 3, 4)).isEmpty shouldEqual false
    Chain(1, 2, 3).isEmpty shouldEqual false
  }

  "toList" should "transform chain to list" in {
    Chain(1, 2, 3, 4, 5, 6).toList shouldEqual List(1, 2, 3, 4, 5, 6)
  }

  "toSet" should "transform chain to set" in {
    Chain(1, 2, 3, 4, 5, 6, 6, 6, 6).toSet shouldEqual Set(1, 2, 3, 4, 5, 6)
  }

  "toString" should "transform chain to string" in {
    Chain(1, 2, 3, 4, 5, 6).toString shouldEqual "Chain(1,2,3,4,5,6)"
  }

  "equals" should "check whether two chains are equal" in {
    Chain(1, 2, 3, 4).equals(Chain(1, 2, 3, 4)) shouldBe true
    Chain(1, 2, 3, 4).equals(1) shouldBe false
    Chain(1).equals(Singleton(1)) shouldBe true
  }

  "hashCode" should "be equal when two chain are equal" in {
    Append(Append(Singleton(1), Singleton(2)), Singleton(3)).hashCode.shouldEqual(
      Append(Singleton(1), Append(Singleton(2), Singleton(3))).hashCode)
  }

  "listify" should "make chains look like lists" in {
    Append(Append(Singleton(1), Singleton(2)), Append(Singleton(3), Singleton(4))).listify.shouldEqual(
      Append(Singleton(1), Append(Singleton(2), Append(Singleton(3), Singleton(4)))))
  }

  "min" should "return min element" in {
    Chain(2, 3, 4, 1, 5, 6).min shouldEqual 1
    Chain(2, 3, 4, 5, 6).min shouldEqual 2
  }

  "max" should "return max element" in {
    Chain(2, 3, 4, 1, 5, 6).max shouldEqual 6
    Chain(2, 3, 4, 1, 3).max shouldEqual 4
  }
}
