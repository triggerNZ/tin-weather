package triggernz.weather

import triggernz.weather.util.{NonEmptyVector => NEV}
import triggernz.weather.image.Image
import java.awt.image.BufferedImage

import scalaz.Comonad
import Globe._

import scala.reflect.ClassTag

sealed trait Globe[A] {
  val latCount: Int
  val lngCount: Int

  def apply(coord: RectCoord): A

  lazy val degreesPerLatitude: Degrees = HalfCircle / latCount
  lazy val degreesPerLongitude: Degrees = FullCircle / lngCount

  def apply(lat: Int, lng: Int): A = apply(RectCoord(lat, lng))

  def apply(lat: Degrees, lng: Degrees): A = {
    val zeroLatIdx: Int = latCount / 2
    val offsetFromZeroLatIdx: Int = (lat / degreesPerLatitude).toInt
    val lngIdx: Int = (lng / degreesPerLongitude).toInt
    apply(RectCoord(zeroLatIdx + offsetFromZeroLatIdx, lngIdx))
  }

  def toNEV: NEV[NEV[A]] = {
    def longitudeAsNev(lat: Int): NEV[A] = {
      val head = apply(RectCoord(lat, 0))
      val tail = for (i <- 1 until lngCount) yield apply(RectCoord(lat, i))
      NEV(head, tail.toVector)
    }

    val head = longitudeAsNev(0)
    val tail = for (i <- 1 until latCount) yield longitudeAsNev(i)

    NEV(head, tail.toVector)
  }

  def toFlatArray[B >: A : ClassTag]: Array[B] = (for {
    lat <- 0 until latCount
    lng <- 0 until lngCount
  } yield apply(lat, lng)).toArray

  def map[B](g: A => B): Globe[B] = new GlobeF(rectCoord => g(apply(rectCoord)), latCount, lngCount)

  def cursor: GlobeCursor[A] = GlobeCursor(0, 0, this)
  def allCursors: Globe[GlobeCursor[A]] =
    new GlobeF(coord => GlobeCursor(coord.lat, coord.lng, this), latCount, lngCount)
}



object Globe {
  // Looks unsafe because index ranges are difficult to prove via types. However it is safe on the outside,
  // i.e. NEV[NEV[A]] => Globe[A] is always a valid operation
  // TODO: Hammer this with property tests
  def fromNev2d[A](nev: NEV[NEV[A]]): Globe[A] = {
    def normalise(idx: Int, length: Int) = (idx + length) % length

    val lats = nev.length
    val lngs = nev.head.length
    new GlobeF({ coord =>
      nev.unsafeGet(normalise(coord.lat, lats)).unsafeGet(normalise(coord.lng, lngs))
      nev.unsafeGet(normalise(coord.lat, lats)).unsafeGet(normalise(coord.lng, lngs))
    }, lats, lngs)
  }

  //As above
  def fromArray[A](arr: Array[A], lats: Int, lngs: Int): Globe[A] =
    new GlobeArr(arr, lats, lngs)

  def HalfCircle = Degrees(180)
  def FullCircle = Degrees(360)

  case class RectCoord(lat: Int, lng: Int)
  case class SpherCoord(lat: Degrees, lng: Degrees)

  // Represents values of A distributed along a sphere.
  // Outer vector represents north to south, inner vectors are west to east.
  private final class GlobeF[A](val f: RectCoord => A, val latCount: Int, val lngCount: Int) extends Globe[A] {
    override def apply(coord: RectCoord): A = f(coord)

    override def toString() = "GlobeF(<function>," + latCount + ", " + lngCount + ")"
  }

  private final class GlobeArr[A](val arr: Array[A], val latCount: Int, val lngCount: Int) extends Globe[A] {
    def apply(coord: RectCoord) = arr(coord.lat * latCount + coord.lng)

    override def toFlatArray[B >: A : ClassTag]: Array[B] =
      arr.asInstanceOf[Array[B]] //Unsafe but will work if A = B. Is fast
  }
}

final case class Degrees(value: Double) extends AnyVal {
  def / (ratio: Int) = Degrees(value / ratio)
  def / (otherDegrees: Degrees): Double = value / otherDegrees.value
}

final case class GlobeCursor[A](lat: Int, lng: Int, globe: Globe[A]) {
  def map[B](f: A => B) = GlobeCursor[B](lat, lng, globe.map(f))
  def extract: A =
    globe(lat, lng)

  def duplicate: GlobeCursor[GlobeCursor[A]] = GlobeCursor(lat, lng, globe.allCursors)
}

object GlobeCursor {
  implicit val comonad = new Comonad[GlobeCursor] {
    override def copoint[A](p: GlobeCursor[A]): A =
      p.extract

    override def cobind[A, B](fa: GlobeCursor[A])(f: GlobeCursor[A] => B): GlobeCursor[B] = ???

    override def map[A, B](fa: GlobeCursor[A])(f: A => B): GlobeCursor[B] =
      fa.map(f)
  }
}