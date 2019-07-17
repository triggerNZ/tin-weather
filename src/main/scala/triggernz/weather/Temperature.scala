package triggernz.weather

case class Temperature(kelvin: Double) extends AnyVal {
  def toCelsius = kelvin - Temperature.ZeroCelsius

  def <(other: Temperature) =
    kelvin < other.kelvin


  def *(factor: Double) =
    Temperature(kelvin * factor)

  def -(other: Temperature) =
    Temperature(kelvin - other.kelvin)

  override def toString = s"${toCelsius} ˚C"
}

object Temperature {
  val LowlandCool = 0.15
  val MountainCool = 0.4
  val OceanCool = 0.3

  val LowlandHeat = 1.3
  val OceanHeat = 0.3
  val MountainHeat = 0.5

  val ZeroCelsius = 273.15

  def celsius(c: Double): Temperature =
    Temperature(ZeroCelsius + c)


  // This is fudged to kinda make sense. Oceans cool and heat slowly, Lowlands cool and heat quickly. Mountains cool
  // quickly and heat slowly
  def updateTemperature(old: Temperature, terrain: Terrain, solarRadiation: SolarRadiation, cloud: Cloud, dt: Hours): Temperature = {
    // Cloud can block out 50% of sun at most
    val cloudAdjustedSolarRadiation = solarRadiation.value - (cloud.percent * solarRadiation.value * 0.5)
    terrain match {
      case Terrain.Lowland =>
        Temperature(old.kelvin + (cloudAdjustedSolarRadiation * LowlandHeat - LowlandCool) * dt.value)
      case Terrain.Mountains =>
        Temperature(old.kelvin + (cloudAdjustedSolarRadiation * MountainHeat - MountainCool) * dt.value)
      case Terrain.Sea =>
        Temperature(old.kelvin + (cloudAdjustedSolarRadiation * OceanHeat - OceanCool) * dt.value)
    }
  }

  def initialTemperatureGlobe(latCount: Int, lngCount: Int, equatorTemperature: Temperature, poleTemperature: Temperature): Globe[Temperature] =
    Globe.ofCoordinates(latCount, lngCount).map { case (lat, _) =>
      val range = equatorTemperature.kelvin - poleTemperature.kelvin
      val diff = (Math.abs(lat.value) / 90) * range
      Temperature(equatorTemperature.kelvin - diff)
    }

}
