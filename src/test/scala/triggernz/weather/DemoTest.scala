package triggernz.weather

import utest._
// Not tests but useful to have them run as tests anyway
object DemoTest extends TestSuite {
  override def tests = Tests {
    'sydney - {
      val (lat, lng) = Demo.cities("Sydney")

      'sunshineProgression - {
        (0 to 24).foreach { i =>
//          println(Demo.sinSunshine(DayOfYear(0), Hours(i))(lat, lng))
        }
      }

      'temperatureProgression - {
        val initial = time("InitialTemp", Demo.initialTemperature)

        var state = initial
        (0 to 96).foreach { i =>
          state = Demo.iterate(state, Hours(i))
//          println(state(lat, lng))
        }
      }
    }
  }

  private def time[A](name: String, a: => A): A = {
    println(s"Starting ${name}")
    val before = System.currentTimeMillis()
    val aValue = a
    val time = System.currentTimeMillis() - before
    println(s"$name took ${time}ms")
    aValue
  }
}
