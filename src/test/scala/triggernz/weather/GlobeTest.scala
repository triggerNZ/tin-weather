package triggernz.weather

import triggernz.weather.util.{NonEmptyVector => NEV}

import utest._

import nyaya.prop._
import nyaya.gen._

import nyaya.test.PropTest._

object GlobeTest extends TestSuite{
  val singleItemGlobe = Globe.fromNev2d(NEV(NEV("One value")))
  val fourPieceGlobe = Globe.fromNev2d(NEV(NEV(0.0,1.0), NEV(2.0, 3.0)))
  val latlongGlobe = Globe.ofCoordinates(180, 360)

  override def tests = Tests {
    'map - {
      singleItemGlobe.map(_.toUpperCase).toNEV ==> Globe.fromNev2d(NEV(NEV("ONE VALUE"))).toNEV
      fourPieceGlobe.map(_ * 2).toNEV ==> Globe.fromNev2d(NEV(NEV(0.0, 2.0), NEV(4.0, 6.0))).toNEV
    }

    'zipWith - {
      fourPieceGlobe.zipWith[Double, Double](fourPieceGlobe, (a, b) => a * b).toNEV ==> Globe.fromNev2d(NEV(NEV(0.0, 1.0), NEV(4.0, 9.0))).toNEV
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

