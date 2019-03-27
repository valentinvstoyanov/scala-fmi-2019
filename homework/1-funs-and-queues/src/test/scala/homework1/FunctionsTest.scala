package homework1

import org.scalatest.{FlatSpec, Matchers}
import Functions._

class FunctionsTest extends FlatSpec with Matchers {
  "fromDigits" should "form a decimal number" in {
    fromDigits(List(1, 2, 3)) shouldBe 123
    fromDigits(List(1, 0, 2, 3, 4, 5, 9)) shouldBe 1023459
    fromDigits(List(0)) shouldBe 0
    fromDigits(List(9, 9, 9, 9, 9)) shouldBe 99999
  }

  it should "form a hex number" in {
    fromDigits(List(1, 12, 4), 16) shouldBe 452
    fromDigits(List(1, 0, 15, 9, 8, 7), 16) shouldBe 1112455
    fromDigits(List(0), 16) shouldBe 0
    fromDigits(List(15, 15, 15,15), 16) shouldBe 65535
  }

  it should "form an octal number" in {
    fromDigits(List(1, 0, 5, 7, 1), 8) shouldBe 4473
    fromDigits(List(0), 8) shouldBe 0
    fromDigits(List(7, 7, 7, 7, 7), 8) shouldBe 32767
  }

  it should "form a binary number" in {
    fromDigits(List(1, 0, 0, 0), 2) shouldBe 8
    fromDigits(List(0), 2) shouldBe 0
    fromDigits(List(1), 2) shouldBe 1
    fromDigits(List(1, 1, 1), 2) shouldBe 7
  }

  "parseInteger" should "parse a decimal number" in {
    parseInteger("123") shouldBe 123
    parseInteger("-45") shouldBe -45
    parseInteger("0") shouldBe 0
    parseInteger("9999") shouldBe 9999
    parseInteger("456789") shouldBe 456789
  }

  it should "parse a hex number" in {
    parseInteger("1C4", 16) shouldBe 452
    parseInteger("0", 16) shouldBe 0
    parseInteger("ABCD", 16) shouldBe 43981
    parseInteger("-ABCD", 16) shouldBe -43981
    parseInteger("ZZZZZ", 16) shouldBe 2446675
  }

  it should "parse an octal number" in {
    parseInteger("10571", 8) shouldBe 4473
    parseInteger("-10571", 8) shouldBe -4473
    parseInteger("0", 8) shouldBe 0
    parseInteger("77777", 8) shouldBe 32767
  }

  "zipMap" should "transform two lists" in {
    zipMap(List(1, 2, 3), List(4, 5, 6), _ * _) shouldBe List(4, 10, 18)
    zipMap(List(3, 6), List(20, 30, 40), (x, y) => y - x) shouldBe List(17, 24)
    zipMap(List.range(1, 2001), List.range(2000, 4002), (x, y) => y - x) shouldBe List.fill(2000)(1999)
    zipMap(Nil, Nil, _ + _) shouldBe Nil
    zipMap(List(1, 2), Nil, _ + _) shouldBe Nil
    zipMap(Nil, List(1, 2), _ + _) shouldBe Nil
  }

  "countCoinChangeVariants" should "count the ways to give a change" in {
    countCoinChangeVariants(List(1, 2, 5), 6) shouldBe 5
  }

  "bfsTraversal" should "traverse the graph" in {
    val map = Map(1 -> List(2, 5, 8),
      2 -> List(1, 3, 6),
      3 -> List(2, 4),
      4 -> List(3),
      5 -> List(6),
      6 -> List(7),
      7 -> List(8),
      8 -> List(9),
      9 -> List())

    bfsTraversal(1, 6, map).toList shouldBe List(1, 2, 5, 8, 3, 6)
    bfsTraversal(4, 6, map).toList shouldBe List(4, 3, 2, 1, 6)
  }
}
