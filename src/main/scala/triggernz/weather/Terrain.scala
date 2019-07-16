package triggernz.weather

sealed trait Terrain
object Terrain {
  case object Sea extends Terrain
  case object Lowland extends Terrain
  case object Mountains extends Terrain

  def elevationToTerrain(elevation: Double) =
    if (elevation < 0.0003)
      Sea
    else if (elevation > 0.5)
      Mountains
    else
      Lowland
}