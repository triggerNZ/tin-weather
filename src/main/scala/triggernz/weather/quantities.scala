package triggernz.weather

final case class Hours(value: Double) extends AnyVal {
  def - (other: Hours): Hours = Hours(value - other.value)
}


final case class Degrees(value: Double) extends AnyVal {
  def + (otherDegrees: Degrees) = Degrees(value + otherDegrees.value)
  def - (otherDegrees: Degrees) = Degrees(value - otherDegrees.value)
  def * (factor: Double): Degrees = Degrees(value * factor)
  def / (ratio: Int) = Degrees(value / ratio)
  def / (otherDegrees: Degrees): Double = value / otherDegrees.value

  def radians: Double = Math.toRadians(value)
}

object Degrees {
  val RightAngle = Degrees(90)
}

final case class DayOfYear(day: Int) extends AnyVal