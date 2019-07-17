package triggernz.weather

import utest._

object SolarRadiationTest extends TestSuite {
  val Equinox = DayOfYear(81)
  val NewYear = DayOfYear(0)

  override def tests = Tests {
    'declinationAngleOnEquinoxIsZero - {
      assert(SolarRadiation.declinationAngle(Equinox).value < 0.001) // Rounding errors
    }

    'sinSolar - {
      'zerozeroEquinox - {
        SolarRadiation.sinSolarElevation(Degrees(0), Degrees(0), Equinox, Hours(0)) ==> 0.0  //Midnight, no sun
        SolarRadiation.sinSolarElevation(Degrees(0), Degrees(0), Equinox, Hours(6 - 0.1)) ==> 0.0  //Before dawn, no sun
        SolarRadiation.sinSolarElevation(Degrees(0), Degrees(0), Equinox, Hours(12)) ==> 1.0 //Midday, full sun
        SolarRadiation.sinSolarElevation(Degrees(0), Degrees(0), Equinox, Hours(18 + 0.1)) ==> 0.0 //After Sunset, no sun
        SolarRadiation.solarRadiationGlobe(180, 360, Equinox, Hours(12))(Degrees(0), Degrees(0)).value ==> 1.0
      }

      'oppositeSideOfTheEarthEquinox - {
        SolarRadiation.sinSolarElevation(Degrees(180), Degrees(0), Equinox, Hours(12)) ==> 0.0
      }
    }

    'sinSolarElevationGlobe - {
      'newYearSouthSunnierThanNorth - {
        val globe = SolarRadiation.solarRadiationGlobe(180, 360, NewYear, Hours(12))
        val fixedLng = Degrees(0)
        for (latInt <- 1 to 90) {
          val latSouth = Degrees(latInt)
          val latNorth = Degrees(-latInt)

          assert(globe(latNorth, fixedLng).value < globe(latSouth, fixedLng).value)
        }
      }
      'equinoxSymmetry - {
        val globe = SolarRadiation.solarRadiationGlobe(180, 360, Equinox, Hours(12))
        val fixedLng = Degrees(0)

        // As we move from the equator (both north and south, the radiation should be decreasing)
        for (latInt <- 1 to 90) {
          val lastLatSouth = Degrees(latInt - 1)
          val lastLatNorth = Degrees(-(latInt - 1))

          val latSouth = Degrees(latInt)
          val latNorth = Degrees(-latInt)

          assert(globe(latSouth, fixedLng).value < globe(lastLatSouth, fixedLng).value)
          assert(globe(latNorth, fixedLng).value < globe(lastLatNorth, fixedLng).value)

          for (lngInt <- 1 to 89) {
            val lastLngEast = Degrees(lngInt - 1)
            val lastLngWest = Degrees(-(lngInt - 1))

            val lngEast = Degrees(-lngInt)
            val lngWest = Degrees(lngInt)

            assert(globe(latSouth, lngEast).value < globe(lastLatSouth, lastLngEast).value)
            assert(globe(latNorth, lngWest).value < globe(lastLatNorth, lastLngWest).value)
          }

          //Other side of the earth should be pitch black
          for (lngInt <- 90 to 189) {
            val lngEast = Degrees(-lngInt)
            val lngWest = Degrees(lngInt)
            assert(globe(latSouth, lngEast).value < 0.001)
            assert(globe(latSouth, lngWest).value < 0.001)
          }
        }
      }
      'smoothAlongLongitude - {
        val globe = SolarRadiation.solarRadiationGlobe(180, 360, NewYear, Hours(12))
        globe(Degrees(-45), Degrees(-1)) ==> globe(Degrees(-45), Degrees(1))
      }

    }
  }

}
