package homework2

import scala.annotation.tailrec

sealed trait Chain[+A] {
  def head: A

  def tail: Option[Chain[A]]

  def isEmpty: Boolean = false

  def +:[B >: A](front: B): Chain[B] = Append(Singleton(front), this)

  def :+[B >: A](back: B): Chain[B] = Append(this, Singleton(back))

  def ++[B >: A](right: Chain[B]): Chain[B] = Append(this, right)

  def foldLeft[B](initial: B)(f: (B, A) => B): B = {
    @tailrec
    def foldLeftRec(c: Chain[A], acc: B): B = c match {
      case Singleton(first) => f(acc, first)
      case Append(Singleton(first), rest) => foldLeftRec(rest, f(acc, first))
      case _ => sys.error("Unexpected listify format")
    }

    foldLeftRec(this.listify, initial)
  }

  def reduceLeft[B >: A](f: (B, A) => B): B = this.listify match {
    case Singleton(first) => first
    case Append(Singleton(first), rest) => rest.foldLeft(first: B)(f)
    case _ => sys.error("Unexpected listify format")
  }

  def map[B](f: A => B): Chain[B] = this.tail match {
    case None => Chain(f(head))
    case Some(tail) => tail.foldLeft(Chain(f(this.head)))(_ :+ f(_))
  }

  def flatMap[B](f: A => Chain[B]): Chain[B] = this.tail match {
    case None => Chain(f(head).head)
    case Some(tail) => tail.foldLeft(f(this.head))(_ ++ f(_))
  }

  def foreach(f: A => Unit): Unit = foldLeft(())((_, next) => f(next))

  override def equals(that: Any): Boolean = that match {
    case c: Chain[_] => this.toList == c.toList
    case _ => false
  }

  override def hashCode: Int = foldLeft(0) {
    _ * 31 + _.hashCode
  }

  override def toString: String = toList.mkString("Chain(", ",", ")")

  def toList: List[A] = foldLeft(List.empty[A])((acc, next) => next :: acc).reverse

  def toSet[B >: A]: Set[B] = foldLeft(Set.empty[B])((acc, next) => acc + next)

  def min[B >: A](implicit order: Ordering[B]): B = this.reduceLeft((a, b) => if (order.lteq(a, b)) a else b)

  def max[B >: A](implicit order: Ordering[B]): B = this.reduceLeft((a, b) => if (order.gteq(a, b)) a else b)

  def listify: Chain[A] = this match {
    case singleton@Singleton(_) => singleton
    case append@Append(Singleton(_), Singleton(_)) => append
    case Append(singleton@Singleton(_), right) => Append(singleton, right.listify)
    case Append(left, right) => Append(Singleton(left.head), (left.tail.get ++ right).listify)
  }
}

case class Singleton[+A](head: A) extends Chain[A] {
  override def tail: Option[Chain[A]] = None
}

case class Append[+A](left: Chain[A], right: Chain[A]) extends Chain[A] {
  def head: A = left.head

  def tail: Option[Chain[A]] = left match {
    case Singleton(_) => Some(right)
    case _ => listify.tail
  }
}

object Chain {
  def apply[A](head: A, rest: A*): Chain[A] =
    if (rest.isEmpty) Singleton(head)
    else rest.foldLeft(Singleton(head): Chain[A])((acc, next) => Append(acc, Singleton(next)))

  // Allows Chain to be used in pattern matching
  def unapplySeq[A](chain: Chain[A]): Option[Seq[A]] = Some(chain.toList)
}
