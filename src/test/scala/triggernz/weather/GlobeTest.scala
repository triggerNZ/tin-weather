package triggernz.weather

import triggernz.weather.util.{NonEmptyVector => NEV}
import utest._

object GlobeTest extends TestSuite{
  override def tests = Tests {
    val singleItemGlobe = Globe(NEV(NEV("One value")))
//    val fourPieceGlobe = Globe(NEV(NEV(0,1), NEV(2, 3)))

    'map - {
      singleItemGlobe.map(_.toUpperCase) ==> Globe(NEV(NEV("ONE VALUE")))
    }
  }
}
