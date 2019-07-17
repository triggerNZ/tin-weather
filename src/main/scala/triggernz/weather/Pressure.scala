package triggernz.weather

final case class Pressure(kpa: Double) extends AnyVal {
  def * (factor: Double) = Pressure(kpa * factor)
}

// https://en.wikipedia.org/wiki/Barometric_formula
object Pressure {
  val StaticPressure = Pressure(101.325)
  val MolarMassOfAir = 0.0289644
  val GravitationalAcceleration = 9.80665
  val UniversalGasConstant = 8.3144598

  def pressure(elevation: Double, temperature: Temperature): Pressure =
    StaticPressure * Math.exp(
      (-GravitationalAcceleration * MolarMassOfAir * elevation) / (temperature * UniversalGasConstant).kelvin)
}
