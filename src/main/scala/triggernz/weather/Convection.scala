package triggernz.weather

object Convection {
  type ConvectionFn =
    (GlobeCursor[(Temperature, Pressure, Cloud, Humidity)], Hours) =>
      (Temperature, Pressure, Cloud, Humidity)

  object Factors {
    val Pressure = 20
    val Temperature = 1
    val Cloud = 20
    val Humidity = 20
  }

  def noop(cursor: GlobeCursor[(Temperature, Pressure, Cloud, Humidity)], hour: Hours):
    (Temperature, Pressure, Cloud, Humidity) = cursor.extract

  // The basic idea is: Get a pressure differential and then update values in each direction
  // according to the differential.
  def convection(cursor: GlobeCursor[(Temperature, Pressure, Cloud, Humidity)], hour: Hours):
    (Temperature, Pressure, Cloud, Humidity) = {
    val local = cursor.extract
    val north = cursor.north.extract
    val south = cursor.south.extract
    val east = cursor.east.extract
    val west = cursor.west.extract

    val allDirections =
      singleDirection(north) _ andThen
        singleDirection(east) _ andThen
        singleDirection(south) _ andThen
        singleDirection(west)
    val result = allDirections(local)

    result
  }


  def singleDirection(direction: (Temperature, Pressure, Cloud, Humidity))(local: (Temperature, Pressure, Cloud, Humidity)):
    (Temperature, Pressure, Cloud, Humidity) = {

    val dirDiff = (direction._2.kpa - local._2.kpa)
    var dirFlow = dirDiff / local._2.kpa
    val dirFactor = dirFlow * dirDiff

    val newValues = (
      Temperature(local._1.kelvin + dirFactor * Factors.Temperature),
      Pressure(local._2.kpa + dirFactor * Factors.Pressure),
      Cloud(local._3.percent + dirFactor * Factors.Cloud),
      Humidity(local._4.value + dirFactor * Factors.Humidity)
    )

    newValues
  }

}
