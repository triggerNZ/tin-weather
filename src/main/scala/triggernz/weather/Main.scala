package triggernz.weather


import java.time.ZonedDateTime

import javax.imageio.ImageIO
import triggernz.weather.image.Image

import Simulation._

object Main {

  val demos: Map[String, () => Globe[Byte]] = Map(
    "elevations" -> (() => elevationsImage),
    "elevationsMapped" -> (() => elevationsMappedImage),
    "sinSunshine" -> (() => sinSunshineImage),
    "sinSunshineWithElevations" -> (() => sinSunshineWithElevationsImage),
    "sea" -> (() => seaImage)
  )

  val cities: Map[String, ((Degrees, Degrees), Set[Hours])] =
    Map(
      "Sydney"    -> ((Degrees(33.8688), Degrees(151.2093)), Set(Hours(0), Hours(24), Hours(10 * 24 + 6), Hours(300 * 24 + 6))),
      "Melbourne" -> ((Degrees(37.8136), Degrees(144.9631)), Set(Hours(1), Hours(14 * 24 + 9))),
      "London"    -> ((Degrees(-51.5074), Degrees(-0.1278)), Set[Hours]()),
      "Paris"     -> ((Degrees(-48.8566), Degrees(2.3522)), Set[Hours]()),
      "Auckland"  -> ((Degrees(36.8485), Degrees(174.7633)), Set[Hours]()),
      "Beijing"   -> ((Degrees(-39.9042), Degrees(116.4074)), Set[Hours]()),
      "Fiji"      -> ((Degrees(17.7134), Degrees(178.0650)), Set[Hours]()),
      "Nairobi"   -> ((Degrees(1.2921), Degrees(36.8219)), Set[Hours]()),
      "Las Vegas" -> ((Degrees(-36.1699), Degrees(-115.1398)), Set[Hours]()),
      "Santiago"  -> ((Degrees(33.4489), Degrees(-70.6693)), Set(Hours(1), Hours(22 * 24 + 9), Hours(5 * 24 + 3))),
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

  def conditionString(cloud: Cloud, prec: Precipitation): String = prec match {
    case Precipitation.None if cloud.percent > 0.4 => "Cloudy"
    case Precipitation.None => "Clear"
    case Precipitation.Rain => "Rain"
    case Precipitation.Snow => "Snow"
  }

  def main(args: Array[String]): Unit = {
    args.toList match {
      case "--demoImage" :: name :: Nil =>
        demo(name)
      case "--convection" :: daysStr :: Nil =>
        outputTable(daysStr.toInt, Convection.convection)
      case Nil =>
        outputTable(365, Convection.noop)
      case _ =>
        println("Usage: <no args> | --convection <days> | --demoImage <demo image name>")

    }
  }

  def psvLine(name: String,
              lat: Degrees,
              lng: Degrees,
              elevation: Int,
              localTime: ZonedDateTime,
              conditionString: String,
              temperature: Temperature,
              pressure: Pressure,
              humidity: Humidity) =
    List(
      name,
      (s"${lat.value.toString},${lng.value.toString},${elevation}"),
      localTime.format(LocalTime.Format),
      conditionString,
      temperature.toCelsius,
      (pressure.kpa * 10).toString,
      (humidity.value * 100).toInt.toString
    ).mkString("|")

  def outputTable(days: Int, convectionFunction: Convection.ConvectionFn): Unit = {
    val HoursPerStep = 3
    // Imperative here deliberately. Scala streams aren't great. Given that this is the main loop of the simulation,
    // its acceptable.
    var currentGlobe = initial
    (0 to (days * 24) by HoursPerStep).foreach { hourInt =>
      val hour = Hours(hourInt)
      cities.flatMap { case (name, ((lat, lng), requiredHours)) =>
        // It may be surprising to calculate unneeded states here. However it actually helps us prevent stack overflows
        // and lets Memo do its thing
        val elevation = elevationsInMetres(lat, lng)
        val (temperature, pressure, humidity, cloud, prec) = currentGlobe(lat, lng)

        val conditions = conditionString(cloud, prec)
        val localTime = LocalTime.computeLocalTime(hour, lng)
        if (requiredHours.contains(hour)) {
          Some(psvLine(name, lat, lng, elevation, localTime, conditions, temperature, pressure, humidity))
        } else {
          None
        }
      }.foreach(println)
      currentGlobe = iterate(currentGlobe, hour, Hours(HoursPerStep), convectionFunction)
    }
  }

}
