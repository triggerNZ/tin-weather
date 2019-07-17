package triggernz.weather

import triggernz.weather.Terrain.{Lowland, Mountains, Sea}

case class Humidity(value: Double) extends AnyVal

object Humidity {
  val EvaporationFactor = 2

  def updateHumidity(oldHumidity: Humidity, temperature: Temperature, terrain: Terrain, dt: Hours) = terrain match {
    case Sea =>
      // Some evaporation still happens at below 0. Go down to -5, rather than trying to model molecule
      // energy distributions
      val tempDiff = Math.max(temperature.kelvin - Temperature.ZeroCelsius + 5, 0)
      val ratioToBoiling = tempDiff / 100.0
      val roomForMoreVapour = 1.0 - oldHumidity.value
      val humidityDiff = roomForMoreVapour * ratioToBoiling * EvaporationFactor
      Humidity(oldHumidity.value + humidityDiff)

    case Lowland =>
      oldHumidity
    case Mountains =>
      oldHumidity
  }
}