package triggernz.weather

import triggernz.weather.util.{NonEmptyVector => NEV}
import scalaz.{Comonad, Memo}
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
    val zeroLngIdx: Int = lngCount / 2

    val offsetFromZeroLatIdx: Int = (lat / degreesPerLatitude).toInt
    val offsetFromZeroLngIdx: Int = (lng / degreesPerLongitude).toInt

    val rect = RectCoord(zeroLatIdx + offsetFromZeroLatIdx, zeroLngIdx + offsetFromZeroLngIdx)
    apply(rect)
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

  def map[B](g: A => B): Globe[B] =
    new Lazy(rectCoord => g(apply(rectCoord)), latCount, lngCount)

  def zipWith[B, C](other: Globe[B], f: (A, B) => C): Globe[C] = {
    //TODO: This isn't quite right if the dimensions don't match
    require(latCount == other.latCount)
    require(lngCount == other.lngCount)
    new Lazy[C](rect => f(apply(rect), other.apply(rect)), latCount, lngCount)
  }

  def zip[B](other: Globe[B]): Globe[(A, B)] =
    zipWith[B, (A, B)](other, (a, b) => (a, b))

  def zip3[B, C](other1: Globe[B], other2: Globe[C]): Globe[(A, B, C)] =
    zip(other1.zip(other2)).map {case (a, (b, c)) => (a, b, c)}

  def zip4[B, C, D](other1: Globe[B], other2: Globe[C], other3: Globe[D]): Globe[(A, B, C, D)] =
    zip(other1.zip3(other2, other3)).map {case (a, (b, c, d)) => (a, b, c, d)}

  def cursor: GlobeCursor[A] = GlobeCursor(0, 0, this)
  def allCursors: Globe[GlobeCursor[A]] =
    new Lazy(coord => GlobeCursor(coord.lat, coord.lng, this), latCount, lngCount)
}



object Globe {
  def apply[A](f: RectCoord => A, latCount: Int, lngCount: Int): Globe[A] =
    new Lazy(f, latCount, lngCount)

  // Looks unsafe because index ranges are difficult to prove via types. However it is safe on the outside,
  // i.e. NEV[NEV[A]] => Globe[A] is always a valid operation
  // TODO: Hammer this with property tests
  def fromNev2d[A](nev: NEV[NEV[A]]): Globe[A] = {
    def normalise(idx: Int, length: Int) = (idx + length) % length

    val lats = nev.length
    val lngs = nev.head.length
    new Lazy({ coord =>
      nev.unsafeGet(normalise(coord.lat, lats)).unsafeGet(normalise(coord.lng, lngs))
      nev.unsafeGet(normalise(coord.lat, lats)).unsafeGet(normalise(coord.lng, lngs))
    }, lats, lngs)
  }

  def const[A](a: A, lats: Int, lngs: Int): Globe[A] =
    Globe(_ => a, lats, lngs)

  //As above
  def fromArray[A](arr: Array[A], lats: Int, lngs: Int): Globe[A] = {
    new Eager(arr, lats, lngs)
  }

  def HalfCircle = Degrees(180)
  def FullCircle = Degrees(360)

  case class RectCoord(lat: Int, lng: Int) {
    def latitude(latCount: Int): Degrees = {
      val degreesPerLatitude: Degrees = HalfCircle / latCount

      degreesPerLatitude * (lat - latCount / 2 )
    }


    def longitude(lngCount: Int): Degrees = {
      val degreesPerLongitude: Degrees = FullCircle / lngCount
      degreesPerLongitude * (lng - lngCount / 2 )
    }
  }

  // Represents values of A distributed along a sphere.
  // Outer vector represents north to south, inner vectors are west to east.
  private final class Lazy[A](val f: RectCoord => A, val latCount: Int, val lngCount: Int) extends Globe[A] {
    val memo = Memo.immutableHashMapMemo(f)

    override def apply(coord: RectCoord): A = {
      memo(coord)
    }

    override def toString() = "GlobeF(<function>," + latCount + ", " + lngCount + ")"
  }

  private final class Eager[A](val arr: Array[A], val latCount: Int, val lngCount: Int) extends Globe[A] {
    def apply(coord: RectCoord) = {
      arr(coord.lat * lngCount + coord.lng)
    }

    override def toFlatArray[B >: A : ClassTag]: Array[B] =
      arr.asInstanceOf[Array[B]] //Unsafe but will work if A = B. Is fast
  }

  def ofCoordinates(latCount: Int, lngCount: Int) =
    Globe(rect => (rect.latitude(latCount), rect.longitude(lngCount)), latCount, lngCount)
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

    override def cobind[A, B](fa: GlobeCursor[A])(f: GlobeCursor[A] => B): GlobeCursor[B] =
      fa.duplicate.map(f)

    override def map[A, B](fa: GlobeCursor[A])(f: A => B): GlobeCursor[B] =
      fa.map(f)
  }
}