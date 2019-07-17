package triggernz.weather

sealed trait Precipitation

object Precipitation {
  case object None extends Precipitation
  case object Rain extends Precipitation
  case object Snow extends Precipitation

  val DecloudingFactor = 0.9
  val CloudThreshold = 0.7

  // Calculates precipitation. The eff
  def precipitation(oldCloud: Cloud, temperature: Temperature): (Precipitation, Cloud) =
    if (oldCloud.percent < CloudThreshold)
      (None, oldCloud)
    else if (temperature.kelvin < Temperature.ZeroCelsius)
      (Snow, Cloud(oldCloud.percent * DecloudingFactor))
    else
      (Rain, Cloud(oldCloud.percent * DecloudingFactor))
}
