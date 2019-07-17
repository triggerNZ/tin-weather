package triggernz.weather

import triggernz.weather.image.Image
import scalaz.syntax.comonad._

object Simulation {
  lazy val elevationsImage: Globe[Byte] =
    Image.imageToGlobe(Image.scaleDown(Image.loadUnsafe("gebco_08_rev_elev_21600x10800.png"), 2))

  def elevations: Globe[Double] = elevationsImage.map(Image.byteToPercent)

  // 8850 metres is the height of mt. everest, presumably the highest elevation in the world
  def elevationsInMetres: Globe[Int] = elevations.map(e => (e * 8850).toInt)

  type WeatherSystem = (Temperature, Pressure, Humidity, Cloud, Precipitation)

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

  def initialPrecipitation: Globe[Precipitation] =
    Globe.const(Precipitation.None, elevations.latCount, elevations.lngCount)

  def initial: Globe[WeatherSystem] =
    initialTemperature.zip5(initialPressure, initialHumidity, initialCloud, initialPrecipitation)

  def iterate(last: Globe[WeatherSystem],
              hours: Hours,
              dt: Hours,
              convectionFunction: Convection.ConvectionFn): Globe[WeatherSystem] = {
    val day = DayOfYear((hours.value / 24).toInt) // Yes this isn't really correct. But it's a game
    val hourOfDay = Hours(hours.value - day.day * 24)

    val nonComonadicComponents: Globe[(Terrain, SolarRadiation, Int)] =
      terrain.zip3(sinSunshine(day, hourOfDay), elevationsInMetres)

    val allComponents = last.zip(nonComonadicComponents).map { case (a, (b,c, d)) => (a, b, c, d) }

    allComponents.cursor.cobind { cursor =>
      val ((oldTemp, _, oldHumidity, oldCloud, oldPrec), terrain, solarRadiation, elevation) = cursor.extract

      val (newPrec, cloudAfterPrec) = Precipitation.precipitation(oldCloud, oldTemp)
      val newTemp = Temperature.updateTemperature(oldTemp, terrain, solarRadiation, dt)
      val newPressure = Pressure.pressure(elevation, oldTemp)
      val newHumidity = Humidity.updateHumidity(oldHumidity, oldTemp, terrain, dt)
      val (newCloud, humidityAfterCloud) = Cloud.updateCloud(cloudAfterPrec, newHumidity)

      (newTemp, newPressure, humidityAfterCloud, newCloud, newPrec, terrain, solarRadiation)
    }.cobind {cursor =>
      val (_, _, _, _, prec, terr, rad) = cursor.extract
      val convectedValues = cursor.map({
        case (temp, pres, humid, cloud, _, _, _) => (temp, pres, cloud, humid)
      }).cobind(convectionFunction(_, hours))
      val (temp, press, cloud, humid) = convectedValues.extract
      (temp, press, humid, cloud, prec, terr, rad)
    }.globe.map { case (temp, press, humid, cloud, prec, _, _) => (temp, press, humid, cloud, prec) }
  }

  def terrain: Globe[Terrain] = elevations.map(Terrain.elevationToTerrain)

  def seaImage: Globe[Byte] = terrain.map( {
    case Terrain.Sea => Image.percentToByte(1.0)
    case _ => Image.percentToByte(0.0)
  })

}
