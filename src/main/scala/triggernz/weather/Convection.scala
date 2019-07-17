package triggernz.weather

object Convection {
  def convection(cursor: GlobeCursor[(Pressure, Temperature, Cloud, Humidity)]):
    (Pressure, Temperature, Cloud, Humidity) = {
    val local = cursor.extract
    val north = cursor.north.extract
    val south = cursor.south.extract
    val east = cursor.east.extract
    val west = cursor.west.extract


    ???
  }
}
