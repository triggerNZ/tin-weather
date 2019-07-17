package triggernz.weather


import java.time.format.DateTimeFormatter

import javax.imageio.ImageIO
import triggernz.weather.image.Image
import scalaz.syntax.comonad._

object Demo {
  lazy val elevationsImage: Globe[Byte] =
    Image.imageToGlobe(Image.loadUnsafe("gebco_08_rev_elev_21600x10800.png"))

  def elevations: Globe[Double] = elevationsImage.map(Image.byteToPercent)

  // 8850 metres is the height of mt. everest, presumably the highest elevation in the world
  val elevationsInMetres: Globe[Int] = elevations.map(e => (e * 8850).toInt)

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

  def initialPressure: Globe[Pressure] = (initialTemperature zip elevationsInMetres).map { case (tmp, elev) =>
    Pressure.pressure(elev, tmp)
  }

  def initialHumidity: Globe[Humidity] =
    Globe.const(Humidity(0.2), elevations.latCount, elevations.lngCount)

  def initialCloud: Globe[Cloud] =
    Globe.const(Cloud(0.2), elevations.latCount, elevations.lngCount)

  def initial: Globe[(Temperature, Pressure, Humidity, Cloud)] =
    initialTemperature.zip4(initialPressure, initialHumidity, initialCloud)

  def iterate(last: Globe[(Temperature, Pressure, Humidity, Cloud)],
              hours: Hours,
              dt: Hours): Globe[(Temperature, Pressure, Humidity, Cloud)] = {
    val day = DayOfYear((hours.value / 24).toInt) // Yes this isn't really correct. But it's a game
    val hourOfDay = Hours(hours.value - day.day * 24)

    val nonComonadicComponents: Globe[(Terrain, SolarRadiation)] =
      terrain.zip(sinSunshine(day, hourOfDay))

    val allComponents = last.zip(nonComonadicComponents).map { case (a, (b,c)) => (a, b, c) }

    allComponents.cursor.cobind { cursor =>
      val ((oldTemp, _, oldHumidity, oldCloud), terrain, solarRadiation) = cursor.extract

      val newTemp = Temperature.updateTemperature(oldTemp, terrain, solarRadiation, dt)
      val newPressure = Pressure.pressure(0, oldTemp) //TODO use elevation
      val newHumidity = Humidity.updateHumidity(oldHumidity, oldTemp, terrain, dt)
      val (newCloud, humidityAfterCloud) = Cloud.updateCloud(oldCloud, newHumidity)

      (newTemp, newPressure, humidityAfterCloud, newCloud, terrain, solarRadiation)
    }.globe.map { case (temp, press, humid, cloud, _, _) => (temp, press, humid, cloud) }
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

  val cities: Map[String, ((Degrees, Degrees), Set[Hours])] =
    Map(
      "Sydney"    -> ((Degrees(33.8688), Degrees(151.2093)), Set(Hours(0), Hours(24), Hours(100 * 24 + 6))),
      "Melbourne" -> ((Degrees(37.8136), Degrees(144.9631)), Set(Hours(1), Hours(300 * 24 + 9))),
      "London"    -> ((Degrees(-51.5074), Degrees(-0.1278)), Set[Hours]()),
      "Paris"     -> ((Degrees(-48.8566), Degrees(2.3522)), Set[Hours]()),
      "Auckland"  -> ((Degrees(36.8485), Degrees(174.7633)), Set[Hours]()),
      "Beijing"   -> ((Degrees(-39.9042), Degrees(116.4074)), Set[Hours]()),
      "Fiji"      -> ((Degrees(17.7134), Degrees(178.0650)), Set[Hours]()),
      "Nairobi"   -> ((Degrees(1.2921), Degrees(36.8219)), Set[Hours]()),
      "Las Vegas" -> ((Degrees(-36.1699), Degrees(-115.1398)), Set[Hours]()),
      "Santiago"  -> ((Degrees(33.4489), Degrees(-70.6693)), Set(Hours(1), Hours(300 * 24 + 9), Hours(5 * 24 + 3))),
      "Dubai"     -> ((Degrees(-25.2048), Degrees(55.2708)), Set[Hours]()),
      "Out at sea" -> ((Degrees(0), Degrees(0)), Set[Hours](Hours(0), Hours(1), Hours(2), Hours(10)))
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
    else {
      //Simulate 2 years by hour
      var currentGlobe = initial
      (0 to (2 * 365 * 24)).foreach { hourInt =>
        val hour = Hours(hourInt)
        cities.flatMap { case (name, ((lat, lng), requiredHours)) =>
          // It may be surprising to calculate unneeded states here. However it actually helps us prevent infinite
          // recursion and lets Memo do its thing
          val elevation = elevationsInMetres(lat, lng)
          val (temperature, pressure, humidity, cloud) = currentGlobe(lat, lng)

          val conditions = "<CONDITIONS-TODO>"
          val localTime = LocalTime.computeLocalTime(hour, lng)
          if (requiredHours.contains(hour)) {
            Some(List(
              name,
              (s"${lat.value.toString},${lng.value.toString},${elevation}"),
              localTime.format(LocalTime.Format),
              conditions,
              temperature.toCelsius,
              (pressure.kpa * 10).toString,
              (humidity.value * 100).toInt.toString
            ).mkString("|"))
          } else {
            None
          }
        }.foreach(println)
        currentGlobe = Demo.iterate(currentGlobe, hour, Hours(1.0))
      }
    }
  }


}
