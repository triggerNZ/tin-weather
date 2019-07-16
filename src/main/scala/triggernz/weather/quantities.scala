package triggernz.weather

case class Temperature(kelvin: Double) extends AnyRef {
  def heatTransfer(otherBody: Temperature, dt: Time): Temperature = {
    Temperature((kelvin - otherBody.kelvin) * dt.seconds)
  }
}
object Temperature {
  val CoolingConstant: Double = 0.1
}
case class Time(seconds: Double) extends AnyRef


