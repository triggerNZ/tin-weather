package triggernz.weather

case class Cloud(percent: Double) extends AnyVal


object Cloud {
  val HumidityToCloudFactor = 0.3

  // Stupidly simple model. Clouds take x% of the humidity below
  def updateCloud(oldCloud: Cloud, humidity: Humidity): (Cloud, Humidity) = {
    val transferredHumidity = humidity.value * HumidityToCloudFactor
    val newCloud = Cloud(Math.min(1.0, oldCloud.percent + transferredHumidity))
    val newHumidity = Humidity(humidity.value - transferredHumidity)
    (newCloud, newHumidity)
  }
}
