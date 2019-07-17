package triggernz.weather

import utest._
// Not all of these are tests but useful to have them run as tests anyway. Uncomment printlns to see output
object DemoTest extends TestSuite {
  override def tests = Tests {
    'sydney - {
      val ((lat, lng), _) = Demo.cities("Sydney")

      'terrain - {
        Demo.terrain(lat, lng) ==> Terrain.Lowland
      }

      'sunshineProgression - {
        (0 to 24).foreach { i =>
//          println(Demo.sinSunshine(DayOfYear(0), Hours(i))(lat, lng))
        }
      }

      'temperatureProgression - {
        val initial = Demo.initialTemperature zip Demo.initialPressure

        var state = initial
        (0 to 96).foreach { i =>
          state = Demo.iterate(state, Hours(i), 1)
//          println(state(lat, lng))
        }
      }
    }
  }
}
