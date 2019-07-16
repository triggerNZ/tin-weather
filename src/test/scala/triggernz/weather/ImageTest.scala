package triggernz.weather

import utest._
import image._
import scalaz.zio.DefaultRuntime

object ImageTest extends TestSuite {
  val runtime = new DefaultRuntime {}
  override def tests = Tests {
    'getRgbAndGlobeReturnSameResult - {
      val img = runtime.unsafeRun(Image.load("gebco_08_rev_elev_21600x10800.png"))
      val globe = Image.imageToGlobe(img)

      'example - {
        println(globe.latCount)
        println(globe.lngCount)
        (img.getRGB(0, 1114) & 0xFF) ==> globe(0, 1114)
      }

//      'all - {
//        for {
//          lat <- (0 until img.getHeight())
//          lng <- (0 until img.getWidth())
//        } assert((img.getRGB(lat, lng) & 0xFF).toByte == globe(lat, lng))
//      }
    }
  }
}
