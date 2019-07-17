package triggernz.weather

case class SolarRadiation(value: Double) extends AnyVal

// https://www.researchgate.net/profile/Mohamad_Kharseh/post/What_is_the_extra-terrestrial_solar_radiance_F0/attachment/59d63e0ac49f478072ea8d7b/AS%3A273765705945088%401442282238044/download/Solar+Radiation+Calculation.pdf
object SolarRadiation {
  private val MaxDeclination = Degrees(23.45)
  private val TwoPi = Math.PI * 2

  val Noon = Hours(12)
  val SolarDegreesPerHour = Degrees(15)

  def declinationAngle(day: DayOfYear): Degrees =
    MaxDeclination * -Math.sin((284.0 + day.day) / 365 * TwoPi)


  def solarRadiationGlobe(latCount: Int, lngCount: Int, day: DayOfYear, utcHour: Hours): Globe[SolarRadiation] =
    Globe.ofCoordinates(latCount, lngCount).map { case (lat, lng) =>
      SolarRadiation(sinSolarElevation(lng, lat, day, utcHour))
    }

  def hourAngle(longitude: Degrees, utcHour: Hours): Degrees = {
    val localHour = utcHour - Hours(longitude / SolarDegreesPerHour)
    SolarDegreesPerHour * (localHour - Noon).value
  }

  def sinSolarElevation(longitude: Degrees, latitude: Degrees, day: DayOfYear, utcHour: Hours) = {
    val declinationRadians = declinationAngle(day).radians
    val latitudeRadians = latitude.radians
    val hrAngle: Degrees = hourAngle(longitude, utcHour)
    val result = Math.sin(latitudeRadians) * Math.sin(declinationRadians) +
      Math.cos(latitudeRadians) * Math.cos(declinationRadians) * Math.cos(hrAngle.radians)

    if (result >= 0) result else 0.0
  }

}
