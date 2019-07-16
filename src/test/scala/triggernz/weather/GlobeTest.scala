package triggernz.weather

import triggernz.weather.util.{NonEmptyVector => NEV}
import triggernz.weather.image.Image

import utest._

object GlobeTest extends TestSuite{


  override def tests = Tests {
    val singleItemGlobe = Globe.fromNev2d(NEV(NEV("One value")))
    val fourPieceGlobe = Globe.fromNev2d(NEV(NEV(0.0,1.0), NEV(2.0, 3.0)))

    'map - {
      singleItemGlobe.map(_.toUpperCase).toNEV ==> Globe.fromNev2d(NEV(NEV("ONE VALUE"))).toNEV
      fourPieceGlobe.map(_ * 2).toNEV ==> Globe.fromNev2d(NEV(NEV(0.0, 2.0), NEV(4.0, 6.0))).toNEV
    }

  }
}
