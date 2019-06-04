package homework2

sealed trait Validated[+E, +A] {
  def isValid: Boolean = this match {
    case Valid(_) => true
    case _ => false;
  }

  def getOrElse[B >: A](default: => B): B = this match {
    case Valid(a) => a
    case _ => default
  }

  def orElse[F >: E, B >: A](default: => Validated[F, B]): Validated[F, B] = this match {
    case Valid(_) => this
    case _ => default
  }

  def zip[EE >: E, B](vb: Validated[EE, B]): Validated[EE, (A, B)] = (this, vb) match {
    case (Valid(a), Valid(b)) => Valid((a, b))
    case (Invalid(e1), Invalid(e2)) => Invalid(e1 ++ e2)
    case (Invalid(e), Valid(_)) => Invalid(e)
    case (Valid(_), Invalid(e)) => Invalid(e)
  }

  def map[B](f: A => B): Validated[E, B] = this match {
    case Valid(a) => Valid(f(a))
    case invalid@Invalid(_) => invalid
  }

  def map2[EE >: E, B, R](vb: Validated[EE, B])(f: (A, B) => R): Validated[EE, R] =
    zip(vb).map(pair => f(pair._1, pair._2))

  def flatMap[EE >: E, B](f: A => Validated[EE, B]): Validated[EE, B] = this match {
    case Valid(a) => f(a)
    case invalid@Invalid(_) => invalid
  }

  def fold[B](invalid: Chain[E] => B, valid: A => B): B = this match {
    case Invalid(errors) => invalid(errors)
    case Valid(a) => valid(a)
  }

  def foreach(f: A => Unit): Unit = fold(_ => (), f)
}

case class Valid[+A](a: A) extends Validated[Nothing, A]

case class Invalid[+E](errors: Chain[E]) extends Validated[E, Nothing]

object Invalid {
  def apply[E](error: E): Invalid[E] = Invalid(Chain(error))
}

object Validated {
  def sequence[E, A](xs: List[Validated[E, A]]): Validated[E, List[A]] = {
    def sequenceRec(xs: List[Validated[E, A]], acc: Validated[E, List[A]]): Validated[E, List[A]] = xs match {
      case Nil => acc
      case h :: t => sequenceRec(t, acc.zip(h).map(p => p._1 :+ p._2))
    }

    sequenceRec(xs, Valid(List.empty[A]))
  }

  implicit class ValidatedTuple2[EE, A, B](val tuple: (Validated[EE, A], Validated[EE, B])) extends AnyVal {
    def zip: Validated[EE, (A, B)] = tuple._1.zip(tuple._2)

    def zipMap[R](f: (A, B) => R): Validated[EE, R] = zip.map(t => f(t._1, t._2))
  }

  implicit class ValidatedTuple3[EE, A, B, C]
  (val tuple: (Validated[EE, A], Validated[EE, B], Validated[EE, C])) extends AnyVal {
    def zip: Validated[EE, (A, B, C)] =
      tuple._1.zip(tuple._2.zip(tuple._3)).flatMap(t => Valid((t._1, t._2._1, t._2._2)))

    def zipMap[R](f: (A, B, C) => R): Validated[EE, R] = zip.map(t => f(t._1, t._2, t._3))
  }

  implicit class ValidatedTuple4[EE, A, B, C, D]
  (val tuple: (Validated[EE, A], Validated[EE, B], Validated[EE, C], Validated[EE, D])) extends AnyVal {
    def zip: Validated[EE, (A, B, C, D)] =
      tuple._1.zip((tuple._2, tuple._3, tuple._4).zip).flatMap(t => Valid(t._1, t._2._1, t._2._2, t._2._3))

    def zipMap[R](f: (A, B, C, D) => R): Validated[EE, R] = zip.map(t => f(t._1, t._2, t._3, t._4))
  }

  implicit class ValidatedTuple5[EE, A, B, C, D, E]
  (val tuple: (Validated[EE, A], Validated[EE, B], Validated[EE, C], Validated[EE, D], Validated[EE, E])) extends AnyVal {
    def zip: Validated[EE, (A, B, C, D, E)] =
      tuple._1.zip((tuple._2, tuple._3, tuple._4, tuple._5).zip).flatMap(t => Valid(t._1, t._2._1, t._2._2, t._2._3, t._2._4))

    def zipMap[R](f: (A, B, C, D, E) => R): Validated[EE, R] = zip.map(t => f(t._1, t._2, t._3, t._4, t._5))
  }

  implicit class OptionToValidated[E, A](val option: Option[A]) extends AnyVal {
    def toValidated(onEmpty: => E): Validated[E, A] = option match {
      case Some(value) => Valid(value)
      case None => Invalid(onEmpty)
    }
  }

}