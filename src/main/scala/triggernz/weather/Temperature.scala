package triggernz.weather

case class Temperature(kelvin: Double) extends AnyVal {
  def toCelsius = kelvin - Temperature.ZeroCelsius

  def *(factor: Double) =
    Temperature(kelvin * factor)

  def -(other: Temperature) =
    Temperature(kelvin - other.kelvin)

  override def toString = s"${toCelsius} ˚C"
}

object Temperature {
  val LowlandCool = 0.3
  val MountainCool = 0.7
  val OceanCool = 0.3

  val LowlandHeat = 1.3
  val OceanHeat = 0.3
  val MountainHeat = 0.5

  val ZeroCelsius = 273.15

  def celsius(c: Double): Temperature =
    Temperature(ZeroCelsius + c)


  // This is fudged to kinda make sense. Oceans cool and heat slowly, Lowlands cool and heat quickly. Mountains cool
  // quickly and heat slowly
  def updateTemperature(old: Temperature, terrain: Terrain, solarRadiation: SolarRadiation, dt: Hours): Temperature = terrain match {
    case Terrain.Lowland =>
      Temperature(old.kelvin + (solarRadiation.value * LowlandHeat - LowlandCool) * dt.value)
    case Terrain.Mountains =>
      Temperature(old.kelvin + (solarRadiation.value * MountainHeat - MountainCool) * dt.value)
    case Terrain.Sea =>
      Temperature(old.kelvin + (solarRadiation.value * OceanHeat - OceanCool) * dt.value)
  }

  def initialTemperatureGlobe(latCount: Int, lngCount: Int, equatorTemperature: Temperature, poleTemperature: Temperature): Globe[Temperature] =
    Globe.ofCoordinates(latCount, lngCount).map { case (lat, lng) =>
      val range = equatorTemperature.kelvin - poleTemperature.kelvin
      val diff = (Math.abs(lat.value) / 90) * range
      Temperature(equatorTemperature.kelvin - diff)
    }

}
