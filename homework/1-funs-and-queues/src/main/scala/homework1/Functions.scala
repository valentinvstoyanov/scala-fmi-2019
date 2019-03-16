package homework1

import scala.annotation.tailrec

object Functions {
  def fromDigits(digits: List[Int], radix: Int = 10): Int = {
    @tailrec def fromDigitsRec(digits: List[Int], acc: Int = 0): Int = digits match {
      case Nil => acc
      case head :: tail => fromDigitsRec(tail, acc + (head * math.pow(radix, tail.length).intValue()))
    }

    fromDigitsRec(digits)
  }

  def parseInteger(integer: String, radix: Int = 10): Int = {
    if (integer.head == '-') -parseInteger(integer.tail, radix)
    else fromDigits(integer.toList.map(_.asDigit), radix)
  }

  def zipMap(a: List[Int], b: List[Int], f: (Int, Int) => Int): List[Int] = {
    @tailrec def zipMapRec(a: List[Int], b: List[Int], acc: List[Int] = Nil): List[Int] = (a, b) match {
      case (_, Nil) => acc.reverse
      case (Nil, _) => acc.reverse
      case (h1 :: t1, h2 :: t2) => zipMapRec(t1, t2, f(h1, h2) :: acc)
    }

    zipMapRec(a, b)
  }

  def countCoinChangeVariants(denominations: List[Int], change: Int): Int = ???

  def bfsTraversal(start: Int, end: Int, neighbours: Int => List[Int]): Queue = ???
}
