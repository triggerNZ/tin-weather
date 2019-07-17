package triggernz.weather

object FixTemperatureToRange {
  def fix(value: Temperature, min: Temperature, max: Temperature) =
    if (value < min)
      min
    else if (max < value)
      max
    else
      value

  // At equator, max would be 60C.
  // At poles 15C.
  // reasonable
  def maxTemperature(latitude: Degrees): Temperature = {
    Temperature(Temperature.ZeroCelsius + 60 - Math.abs(latitude.value) / 2)
  }

  // At equator, min would be 0.
  // At poles -45.
  // okayish
  def minTemperature(latitude: Degrees): Temperature = {
    Temperature(Temperature.ZeroCelsius - Math.abs(latitude.value) / 2)
  }

  def minTemperatureGlobe(latCount: Int, lngCount: Int): Globe[Temperature] =
    Globe.ofCoordinates(latCount, lngCount).map { case (lat, _) => minTemperature(lat)}

  def maxTemperatureGlobe(latCount: Int, lngCount: Int): Globe[Temperature] =
    Globe.ofCoordinates(latCount, lngCount).map { case (lat, _) => maxTemperature(lat)}

  // Look! We can have globes of functions! Yay FP
  def fixTemperatureGlobe(latCount: Int, lngCount: Int): Globe[Temperature => Temperature] = {
    val minmax = minTemperatureGlobe(latCount, lngCount) zip maxTemperatureGlobe(latCount, lngCount)
    minmax.map { case (min, max) => fix (_, min, max)}
  }
}
