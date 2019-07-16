package triggernz.weather

import utest._

object SolarRadiationTest extends TestSuite {
  val Equinox = DayOfYear(81)
  val NewYear = DayOfYear(0)

  override def tests = Tests {
    'declinationAngleOnEquinoxIsZero - {
      assert(SolarRadiation.declinationAngle(Equinox).value < 0.001) // Rounding errors
    }

    'maximumSunshine - {
      'equatorEquinox - {
        SolarRadiation.maximumSunlight(Degrees(0), Equinox) ==> 1.0
      }

      'polesEquinox - {
        val north = SolarRadiation.maximumSunlight(Degrees(-90), Equinox)
        val south = SolarRadiation.maximumSunlight(Degrees(90), Equinox)
        assert(south < 0.01)
        assert (north < 0.01)
      }

      'polesNewYear - {
        val north = SolarRadiation.maximumSunlight(Degrees(-90), NewYear)
        val south = SolarRadiation.maximumSunlight(Degrees(90), NewYear)

        assert(south > 0.01)
        assert (north < 0.1)
      }
    }

    'sinSolar - {
      'zerozeroEquinox - {
        SolarRadiation.sinSolarElevation(Degrees(0), Degrees(0), Equinox, Hours(0)) ==> 0.0  //Midnight, no sun
        SolarRadiation.sinSolarElevation(Degrees(0), Degrees(0), Equinox, Hours(6 - 0.1)) ==> 0.0  //Before dawn, no sun
        SolarRadiation.sinSolarElevation(Degrees(0), Degrees(0), Equinox, Hours(12)) ==> 1.0 //Midday, full sun
        SolarRadiation.sinSolarElevation(Degrees(0), Degrees(0), Equinox, Hours(18 + 0.1)) ==> 0.0 //After Sunset, no sun
        SolarRadiation.sinSolarRadiationGlobe(180, 360, Equinox, Hours(12))(Degrees(0), Degrees(0)) ==> 1.0
      }

      'oppositeSideOfTheEarthEquinox - {
        SolarRadiation.sinSolarElevation(Degrees(180), Degrees(0), Equinox, Hours(12)) ==> 0.0
      }
    }

    'maxSunlightGlobe - {
      'newYearSouthSunnierThanNorth - {
        val globe = SolarRadiation.maximumSunlightGlobe(180, 360, NewYear)
        val fixedLng = Degrees(15)
        for (latInt <- 1 to 90) {
          val latSouth = Degrees(latInt)
          val latNorth = Degrees(-latInt)

          assert(globe(latNorth, fixedLng) < globe(latSouth, fixedLng))
        }
      }

      'equinoxSymmetry - {
        val globe = SolarRadiation.maximumSunlightGlobe(180, 360, Equinox)
        val fixedLng = Degrees(15)
        globe(Degrees(0), fixedLng) ==> 1.0

        // As we move from the equator (both north and south, the radiation should be decreasing)
        for (latInt <- 1 to 90) {
          val lastLatSouth = Degrees(latInt - 1)
          val lastLatNorth = Degrees(-(latInt - 1))

          val latSouth = Degrees(latInt)
          val latNorth = Degrees(-latInt)

          assert(globe(latSouth, fixedLng) < globe(lastLatSouth, fixedLng))
          assert(globe(latNorth, fixedLng) < globe(lastLatNorth, fixedLng))

          //As we move east the radiation should stay identical
          for (lngInt <- 1 to 360) {
            val lastLng = Degrees(lngInt - 1)
            val lng = Degrees(lngInt)
            globe(latSouth, lastLng) ==> globe(latSouth, lng)
          }
        }
      }
    }

    'sinSolarElevationGlobe - {
      'newYearSouthSunnierThanNorth - {
        val globe = SolarRadiation.sinSolarRadiationGlobe(180, 360, NewYear, Hours(12))
        val fixedLng = Degrees(0)
        for (latInt <- 1 to 90) {
          val latSouth = Degrees(latInt)
          val latNorth = Degrees(-latInt)

          assert(globe(latNorth, fixedLng) < globe(latSouth, fixedLng))
        }
      }
      'equinoxSymmetry - {
        val globe = SolarRadiation.sinSolarRadiationGlobe(180, 360, Equinox, Hours(12))
        val fixedLng = Degrees(0)

        // As we move from the equator (both north and south, the radiation should be decreasing)
        for (latInt <- 1 to 90) {
          val lastLatSouth = Degrees(latInt - 1)
          val lastLatNorth = Degrees(-(latInt - 1))

          val latSouth = Degrees(latInt)
          val latNorth = Degrees(-latInt)

          assert(globe(latSouth, fixedLng) < globe(lastLatSouth, fixedLng))
          assert(globe(latNorth, fixedLng) < globe(lastLatNorth, fixedLng))

          for (lngInt <- 1 to 89) {
            val lastLngEast = Degrees(lngInt - 1)
            val lastLngWest = Degrees(-(lngInt - 1))

            val lngEast = Degrees(-lngInt)
            val lngWest = Degrees(lngInt)

            assert(globe(latSouth, lngEast) < globe(lastLatSouth, lastLngEast))
            assert(globe(latNorth, lngWest) < globe(lastLatNorth, lastLngWest))
          }

          //Other side of the earth should be pitch black
          for (lngInt <- 90 to 189) {
            val lngEast = Degrees(-lngInt)
            val lngWest = Degrees(lngInt)
            assert(globe(latSouth, lngEast) < 0.001)
            assert(globe(latSouth, lngWest) < 0.001)
          }
        }
      }
      'smoothAlongLongitude - {
        val globe = SolarRadiation.sinSolarRadiationGlobe(180, 360, NewYear, Hours(12))
        globe(Degrees(-45), Degrees(-1)) ==> globe(Degrees(-45), Degrees(1))
      }

    }
  }

}
