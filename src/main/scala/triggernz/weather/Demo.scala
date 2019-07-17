package triggernz.weather


import javax.imageio.ImageIO
import triggernz.weather.image.Image

import scalaz.syntax.comonad._

object Demo {
  lazy val elevationsImage: Globe[Byte] =
    Image.imageToGlobe(Image.loadUnsafe("gebco_08_rev_elev_21600x10800.png"))

  def elevations: Globe[Double] = elevationsImage.map(Image.byteToPercent)

  def elevationsMappedImage = elevationsImage.map(identity)

  def sinSunshine(day: DayOfYear, hourofDay: Hours): Globe[SolarRadiation] =
    SolarRadiation.solarRadiationGlobe(elevationsImage.latCount, elevationsImage.lngCount,day, hourofDay)

  def sinSunshineImage =
    sinSunshine(DayOfYear(0), Hours(12)).map(sr => Image.percentToByte(sr.value))


  def sinSunshineWithElevations =
    elevations.zipWith[SolarRadiation, Double](sinSunshine(DayOfYear(0), Hours(12)), (elevation, sunshine) => elevation * sunshine.value)

  def sinSunshineWithElevationsImage = sinSunshineWithElevations.map(Image.percentToByte)

  def initialTemperature: Globe[Temperature] =
    Temperature.initialTemperatureGlobe(elevations.latCount, elevations.lngCount, Temperature.celsius(30), Temperature.celsius(-10))

  def initial: Globe[Temperature] =
    initialTemperature

  def iterate(last: Globe[Temperature], hours: Hours): Globe[Temperature] = {
    val day = DayOfYear((hours.value / 24).toInt) // Yes this isn't really correct. But it's a game
    val hourOfDay = Hours(hours.value - day.day * 24)

    val nonComonadicComponents: Globe[(Terrain, SolarRadiation)] =
      terrain.zip(sinSunshine(day, hourOfDay))

    val allComponents = last.zip(nonComonadicComponents).map { case (a, (b,c)) => (a, b, c) }

    allComponents.cursor.cobind { cursor =>
      val (oldTemp, terrain, solarRadiation) = cursor.extract

      val newTemp = Temperature.updateTemperature(oldTemp, terrain, solarRadiation)

      (newTemp, terrain, solarRadiation)
    }.globe.map { case (temp, _, _) => temp }
  }

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

  val cities: Map[String, (Degrees, Degrees)] =
    Map(
      "Sydney"    -> (Degrees(33.8688), Degrees(151.2093)),
      "Melbourne" -> (Degrees(37.8136), Degrees(144.9631)),
      "London"    -> (Degrees(-51.5074), Degrees(-0.1278)),
      "Paris"     -> (Degrees(-48.8566), Degrees(2.3522)),
      "Auckland"  -> (Degrees(36.8485), Degrees(174.7633)),
      "Beijing"   -> (Degrees(-39.9042), Degrees(116.4074)),
      "Fiji"      -> (Degrees(17.7134), Degrees(178.0650)),
      "Nairobi"   -> (Degrees(1.2921), Degrees(36.8219)),
      "Las Vegas" -> (Degrees(-36.1699), Degrees(-115.1398)),
      "Santiago"  -> (Degrees(33.4489), Degrees(-70.6693)),
      "Dubai"     -> (Degrees(-25.2048), Degrees(55.2708)),
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
