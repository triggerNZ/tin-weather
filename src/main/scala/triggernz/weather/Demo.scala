package triggernz.weather


import javax.imageio.ImageIO
import triggernz.weather.image.Image

import scalaz.syntax.comonad._

object Demo {
  def elevationsImage: Globe[Byte] =
    Image.imageToGlobe(Image.loadUnsafe("gebco_08_rev_elev_21600x10800.png"))

  def elevations: Globe[Double] = elevationsImage.map(Image.byteToPercent)

  def elevationsMappedImage = elevationsImage.map(identity)

  def sinSunshine: Globe[SolarRadiation] =
    SolarRadiation.solarRadiationGlobe(elevationsImage.latCount, elevationsImage.lngCount, DayOfYear(0), Hours(12))

  def sinSunshineImage =
    sinSunshine.map(sr => Image.percentToByte(sr.value))


  def sinSunshineWithElevations =
    elevations.zipWith[SolarRadiation, Double](sinSunshine, (elevation, sunshine) => elevation * sunshine.value)

  def sinSunshineWithElevationsImage = sinSunshineWithElevations.map(Image.percentToByte)

  def initialTemperature: Globe[Temperature] =
    Temperature.initialTemperatureGlobe(elevations.latCount, elevations.lngCount, Temperature.celsius(30), Temperature.celsius(-15))

  def iterate(last: Globe[(Temperature, Terrain, SolarRadiation)]): Globe[(Temperature, Terrain, SolarRadiation)] =
    last.cursor.cobind { cursor =>
      val (oldTemp, terrain, solarRadiation) = cursor.extract

      val newTemp = Temperature.updateTemperature(oldTemp, terrain, solarRadiation)

      (newTemp, terrain, solarRadiation)
    }.globe

  def terrain: Globe[Terrain] = elevations.map(Terrain.elevationToTerrain)

  def seaImage: Globe[Byte] = terrain.map( {
    case Terrain.Sea => Image.percentToByte(1.0)
    case _ => Image.percentToByte(0.0)
  })


  val demos: Map[String, () => Globe[Byte]] = Map(
    "elevations" -> (() => elevationsImage),
    "elevationsMapped" -> (() => elevationsMappedImage),
    "sinSunshine" -> (() => sinSunshineImage),
    "sinSunshineWithElevations" -> (() => sinSunshineWithElevationsImage),
    "sea" -> (() => seaImage)
  )


  def savePng(g: Globe[Byte], name: String) = {
    val fromGlobe = Image.globeToImage(g)
    val filename = s"${name}.png"
    ImageIO.write(fromGlobe, "PNG", new java.io.File(filename))
    println(s"Wrote ${filename}")
  }


  def demo(name: String): Unit = {
    println(s"Running ${name}")
    val globe = demos(name)
    savePng(globe(), name)
  }

  def main(args: Array[String]): Unit = {
    if (args.length > 0)
      demo(args.head)
    else
      println("Need demo name")
  }
}
