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
    }
  }
}
