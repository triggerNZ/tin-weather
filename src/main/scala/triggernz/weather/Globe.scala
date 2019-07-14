package triggernz.weather

import scalaz.{Foldable, Foldable1, Semigroup}
import triggernz.weather.util.NonEmptyVector

// Represents values of A distributed along a sphere.
// Outer vector represents north to south, inner vectors are west to east.
final case class Globe[+A](values: NonEmptyVector[NonEmptyVector[A]]) {
  import Globe._

  lazy val degreesPerLatitude: Degrees = HalfCircle / values.length
  lazy val degreesPerLongitude: Degrees = FullCircle / values.length

  def apply(lat: Int, lng: Int): Option[A] =
    values(lat).flatMap(_(lng))

  def apply(lat: Degrees, lng: Degrees): Option[A] = {
    val zeroLatIdx: Int = values.length / 2
    val offsetFromZeroLatIdx: Int = (lat / degreesPerLatitude).toInt
    val lngIdx: Int = (lng / degreesPerLongitude).toInt
    apply(zeroLatIdx + offsetFromZeroLatIdx, lngIdx)
  }

  def map[B](f: A => B): Globe[B] = Globe(values.map(_.map(f)))
}

object Globe {
  def HalfCircle = Degrees(180)
  def FullCircle = Degrees(360)

  implicit val foldable = new Foldable1[Globe] {
    private implicit val foldableNEV: Foldable1[NonEmptyVector] = implicitly

    override def foldMap1[A, B](fa: Globe[A])(f: A => B)(implicit S: Semigroup[B]): B = {
      import scalaz.syntax.foldable1._
      fa.values.foldMap1(_.foldMap1(f))
    }


    override def foldMapRight1[A, B](fa: Globe[A])(z: A => B)(f: (A, => B) => B): B = {
     ???
    }
  }
}

final case class Degrees(value: Double) extends AnyVal {
  def / (ratio: Int) = Degrees(value / ratio)
  def / (otherDegrees: Degrees): Double = value / otherDegrees.value
}

final case class GlobeCursor[A](lat: Int, lng: Int, globe: Globe[A]) {
  def extract = globe(lat, lng)
}