package triggernz.weather

import utest._

import nyaya.prop._
import nyaya.gen._

import nyaya.test.PropTest._

object GlobeTest extends TestSuite{
  val singleItemGlobe = Globe.fromVector(Vector(Vector("One value")))
  val fourPieceGlobe = Globe.fromVector(Vector(Vector(0.0,1.0), Vector(2.0, 3.0)))
  val latlongGlobe = Globe.ofCoordinates(180, 360)

  override def tests = Tests {
    'map - {
      singleItemGlobe.map(_.toUpperCase).toVector ==> Globe.fromVector(Vector(Vector("ONE VALUE"))).toVector
      fourPieceGlobe.map(_ * 2).toVector ==> Globe.fromVector(Vector(Vector(0.0, 2.0), Vector(4.0, 6.0))).toVector
    }

    'zipWith - {
      fourPieceGlobe.zipWith[Double, Double](fourPieceGlobe, (a, b) => a * b).toVector ==> Globe.fromVector(Vector(Vector(0.0, 1.0), Vector(4.0, 9.0))).toVector
    }

    'rectCoord - {
      'toLatitude - {
        val equator = Globe.RectCoord(90, 90)
        val northPole = Globe.RectCoord(0, 0)
        equator.latitude(180) ==> Degrees(0)
        northPole.latitude(180) ==> Degrees(-90)
      }
      'toLongitude - {
        val topLeft = Globe.RectCoord(0, 0)
        topLeft.longitude(360) ==> Degrees(-180)

        val bottomRight = Globe.RectCoord(0, 360)
        bottomRight.longitude(360) ==> Degrees(180)
      }
    }

    'properties - {
      'coordsInCoordsOut - testProp(Properties.coordsInEqualCoordsOut, Generators.genLatLng)
    }

    'cursor - {
      'northPoleNorth - {
        GlobeCursor(0, 0, fourPieceGlobe).north ==> GlobeCursor(0, 1, fourPieceGlobe)
      }
      'nonPoleNorth - {
        GlobeCursor(1, 0, fourPieceGlobe).north ==> GlobeCursor(0, 0, fourPieceGlobe)
      }
      'southPoleSouth - {
        GlobeCursor(1, 0, fourPieceGlobe).south ==> GlobeCursor(1, 1, fourPieceGlobe)
      }
      'nonPoleSouth - {
        GlobeCursor(0, 1, fourPieceGlobe).south ==> GlobeCursor(1, 1, fourPieceGlobe)
      }

      'eastAtEnd - {
        GlobeCursor(0, 1, fourPieceGlobe).east ==> GlobeCursor(0, 0, fourPieceGlobe)
      }
      'east - {
        GlobeCursor(0, 0, fourPieceGlobe).east ==> GlobeCursor(0, 1, fourPieceGlobe)
      }

      'westAtStart - {
        GlobeCursor(0, 0, fourPieceGlobe).west ==> GlobeCursor(0, 1, fourPieceGlobe)
      }
      'west - {
        GlobeCursor(0, 1, fourPieceGlobe).east ==> GlobeCursor(0, 0, fourPieceGlobe)
      }
    }
  }

  object Properties {

    type LatLong = (Degrees, Degrees)
    def close(l1: LatLong, l2: LatLong): Boolean = {
      Math.abs(l1._1.value - l2._1.value) < 1
    }

    val coordsInEqualCoordsOut = Prop.test[(Degrees, Degrees)]("Coords in = coords out",  {coord =>
      close(latlongGlobe(coord._1, coord._2), coord)
    })
  }

  object Generators {
    def genLat: Gen[Degrees] = Gen.chooseDouble(-90, 90).map(Degrees.apply)
    def genLng: Gen[Degrees] = Gen.chooseDouble(-180, 180).map(Degrees.apply)

    def genLatLng: Gen[(Degrees, Degrees)] = for {
      lat <- genLat
      lng <- genLng
    } yield (lat, lng)
  }
}

