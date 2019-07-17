package triggernz.weather

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

    val rect = RectCoord(zeroLatIdx - offsetFromZeroLatIdx, zeroLngIdx + offsetFromZeroLngIdx)
    apply(rect)
  }

  def toVector: Vector[Vector[A]] = {
    def longitudeAsVector(lat: Int): Vector[A] =
      (for (i <- 0 until lngCount) yield apply(RectCoord(lat, i))).toVector

    (for {
      i <- 0 until latCount
    } yield longitudeAsVector(i)).toVector
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

  def zip5[B, C, D, E](other1: Globe[B], other2: Globe[C], other3: Globe[D], other4: Globe[E]): Globe[(A, B, C, D, E)] =
    zip(other1.zip4(other2, other3, other4)).map {case (a, (b, c, d, e)) => (a, b, c, d, e)}

  def cursor: GlobeCursor[A] = GlobeCursor(0, 0, this)
  def allCursors: Globe[GlobeCursor[A]] =
    new Lazy(coord => GlobeCursor(coord.lat, coord.lng, this), latCount, lngCount)

  def materialize[B >: A : ClassTag]: Globe[B]
}



object Globe {
  def apply[A](f: RectCoord => A, latCount: Int, lngCount: Int): Globe[A] =
    new Lazy(f, latCount, lngCount)

  // Looks unsafe because index ranges are difficult to prove via types. However it is safe on the outside,
  // i.e. NEV[NEV[A]] => Globe[A] is always a valid operation
  // TODO: Hammer this with property tests
  def fromVector[A](vec: Vector[Vector[A]]): Globe[A] = {
    def normalise(idx: Int, length: Int) = (idx + length) % length

    val lats = vec.length
    val lngs = vec.head.length
    new Lazy({ coord =>
      vec(normalise(coord.lat, lats))(normalise(coord.lng, lngs))
      vec(normalise(coord.lat, lats))(normalise(coord.lng, lngs))
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

      degreesPerLatitude * -(lat - latCount / 2 )
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

    override def materialize[B >: A : ClassTag] = new Eager(toFlatArray, latCount, lngCount)

    override def toString() = "GlobeF(<function>," + latCount + ", " + lngCount + ")"
  }

  private final class Eager[A](val arr: Array[A], val latCount: Int, val lngCount: Int) extends Globe[A] {
    def apply(coord: RectCoord) = {
      arr(coord.lat * lngCount + coord.lng)
    }

    override def toFlatArray[B >: A : ClassTag]: Array[B] =
      arr.asInstanceOf[Array[B]] //Unsafe but will work if A = B. Is fast


    override def materialize[B >: A : ClassTag] =
      this.asInstanceOf[Globe[B]] // Same as above
  }

  def ofCoordinates(latCount: Int, lngCount: Int) =
    Globe(rect => (rect.latitude(latCount), rect.longitude(lngCount)), latCount, lngCount)


}

final case class GlobeCursor[A](lat: Int, lng: Int, globe: Globe[A]) {
  private lazy val halfLng = globe.lngCount/2

  def map[B](f: A => B) = GlobeCursor[B](lat, lng, globe.map(f))
  def extract: A =
    globe(lat, lng)

  def duplicate: GlobeCursor[GlobeCursor[A]] = GlobeCursor(lat, lng, globe.allCursors)

  def north: GlobeCursor[A] =
    if (lat == 0) {
      if (lng > halfLng)
        GlobeCursor(lat, lng - halfLng, globe)
      else
        GlobeCursor(lat, lng + halfLng, globe)

    } else GlobeCursor(lat - 1, lng, globe)

  def south: GlobeCursor[A] =
    if (lat == globe.latCount - 1) {
      if (lng > halfLng)
        GlobeCursor(lat, lng - halfLng, globe)
      else
        GlobeCursor(lat, lng + halfLng, globe)

    } else GlobeCursor(lat + 1, lng, globe)

  def east: GlobeCursor[A] =
    if (lng == globe.latCount - 1)
      GlobeCursor(lat, 0, globe)
    else
      GlobeCursor(lat, lng + 1, globe)

  def west: GlobeCursor[A] =
    if (lng == 0)
      GlobeCursor(lat, globe.lngCount - 1, globe)
    else
     GlobeCursor(lat, lng - 1, globe)

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