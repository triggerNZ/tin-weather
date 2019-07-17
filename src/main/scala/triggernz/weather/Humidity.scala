package triggernz.weather

import triggernz.weather.Terrain.{Lowland, Mountains, Sea}

case class Humidity(value: Double) extends AnyVal

object Humidity {
  val SeaEvaporationFactor = 2
  val NonSeaEvaporationFactor = 0.2 // A little bit of evaporation still comes from rivers, lakes, swimming pools, living beings, etc

  def updateHumidity(oldHumidity: Humidity, temperature: Temperature, terrain: Terrain, dt: Hours) = terrain match {
    case Sea =>
      // Some evaporation still happens at below 0. Go down to -5, rather than trying to model molecule
      // energy distributions
      val tempDiff = Math.max(temperature.kelvin - Temperature.ZeroCelsius + 5, 0)
      val ratioToBoiling = tempDiff / 100.0
      val roomForMoreVapour = 1.0 - oldHumidity.value
      val humidityDiff = roomForMoreVapour * ratioToBoiling * SeaEvaporationFactor
      Humidity(oldHumidity.value + humidityDiff)

    case Lowland | Mountains =>
      Humidity(Math.min(1, oldHumidity.value + oldHumidity.value * NonSeaEvaporationFactor))
  }
}