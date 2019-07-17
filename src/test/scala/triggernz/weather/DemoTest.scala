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

      'convection - {
        import scalaz.syntax.comonad._
        val initial = Demo.initial.
          cursor
          .map {case (t, p, h, c, pp) => (t, p, c, h)}

        val convection = initial.cobind(Convection.convection)
        // Hard to say what the values will be but they should move
        assert(convection.globe(lat, lng) != initial.globe(lat, lng))
      }
    }

    'melbourne - {
      val ((lat, lng), _) = Demo.cities("Melbourne")

      'terrain - {
        Demo.terrain(lat, lng) ==> Terrain.Lowland
      }
    }
  }
}
