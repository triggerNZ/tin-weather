package triggernz.weather.util

import scalaz.{Foldable1, Semigroup}

final case class NonEmptyVector[+A](head: A, tail: Vector[A]) {
  def length = tail.length + 1
  def apply(idx: Int): Option[A] =
    if (idx == 0)
      Some(head)
    else {
      val tailIdx = idx - 1
      if (tailIdx < tail.length)
        Some(tail(tailIdx))
      else
        None
    }

  def map[B](f: A => B) = NonEmptyVector(f(head), tail.map(f))
}

object NonEmptyVector {
  def apply[A](head: A, tail: A*): NonEmptyVector[A] = NonEmptyVector[A](head, tail.toVector)

  implicit val foldable1 = new Foldable1[NonEmptyVector] {
    override def foldMap1[A, B](fa: NonEmptyVector[A])(f: A => B)(implicit F: Semigroup[B]): B =
      fa.tail.foldLeft(f(fa.head))((acc, next) => F.append(acc, f(next)))

    override def foldMapRight1[A, B](fa: NonEmptyVector[A])(z: A => B)(f: (A, =>B) => B): B = {
      fa.tail.foldRight(z(fa.head))((a,b) => f(a,b))
    }
  }
}