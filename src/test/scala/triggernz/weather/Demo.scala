package triggernz.weather

import java.awt.{Image => JImage}
import java.awt.image.BufferedImage

import javax.imageio.ImageIO
import triggernz.weather.image.Image
import scalaz.zio._
import javax.swing.{ImageIcon, JLabel, JOptionPane}

object Demo {
  val elevationsImage: Task[Globe[Byte]] =
    timeTask("Mapping image to globe", Image.load("gebco_08_rev_elev_21600x10800.png").map(Image.imageToGlobe))

  val elevations: Task[Globe[Double]] = elevationsImage.map(_.map(byteToPercent))

  val elevationsMappedImage = elevationsImage.map(_.map(identity))

  val maxDailySunshine: Task[Globe[Double]] = elevations.map { el =>
    SolarRadiation.maximumSunlightGlobe(el.latCount, el.lngCount, DayOfYear(0))
  }

  val maxDailySunshineImage: Task[Globe[Byte]] =
    maxDailySunshine.map(_.map(percentToByte))

  val sinSunshine: Task[Globe[Double]] = elevations.map { el =>
    SolarRadiation.sinSolarRadiationGlobe(el.latCount, el.lngCount, DayOfYear(0), Hours(12))
  }

  val sinSunshineImage = sinSunshine.map(_.map(percentToByte))

  val dailySunshineLitUpElevations = elevations.zip(maxDailySunshine).map {
    case (el, ss) => el.zipWith[Double, Double](ss, (elevation, sunshine) => elevation * sunshine)
  }

  val dailySunshineLitUpElevationsImage = dailySunshineLitUpElevations.map(_.map(percentToByte))

  val sinSunshineWithElevations =  elevations.zip(sinSunshine).map {
    case (el, ss) => el.zipWith[Double, Double](ss, (elevation, sunshine) => elevation * sunshine)
  }

  val sinSunshineWithElevationsImage = sinSunshineWithElevations.map(_.map(percentToByte))

  val terrain: Task[Globe[Terrain]] = elevations.map(_.map(Terrain.elevationToTerrain))

  val seaImage: Task[Globe[Byte]] = terrain.map(_.map {
    case Terrain.Sea => percentToByte(1.0)
    case _ => percentToByte(0.0)
  })

  val demos: Map[String, Task[Globe[Byte]]] = Map(
    "elevations" -> elevationsImage,
    "elevationsMapped" -> elevationsMappedImage,
    "maxDailySunshine" -> maxDailySunshineImage,
    "sinSunshine" -> sinSunshineImage,
    "dailySunshineLitUpElevations" -> dailySunshineLitUpElevationsImage,
    "sinSunshineWithElevations" -> sinSunshineWithElevationsImage,
    "sea" -> seaImage
  )


  def percentToByte(percentage: Double): Byte = ((percentage * 255).toInt & 0xFF).toByte
  def byteToPercent(b: Byte): Double = (b.toInt & 0xFF) / 255.0

  // Doesn't handle failure cases
  def timeTask[A](disp: String, task: Task[A]): Task[A] = {
    val now = Task(System.currentTimeMillis())

    for {
      before <- now
      taskValue <- task
      after <- now
      _ <- Task(println(s"${disp} took ${after - before} ms"))
    } yield taskValue
  }

  def globeToImage(g: Globe[Byte]): Task[BufferedImage] =
    timeTask("Converting globe to image", Task(Image.globeToImage(g)))

  def savePng(g: Globe[Byte], name: String): Task[Unit] =
    for {
      fromGlobe <- globeToImage(g)
      filename = s"${name}.png"
      _ <- Task(ImageIO.write(fromGlobe, "PNG", new java.io.File(filename)))
      _ <- Task(println(s"Wrote ${filename}"))
    } yield ()

  def scaleDown(bi: BufferedImage): Task[JImage] =
    timeTask("Scaling", Task(bi.getScaledInstance(1080, 540, 0)))



  def showMinMax(globe: Globe[Byte]): Task[Unit] = Task {
    val vec = globe.toFlatArray
    println("Min: " + vec.min)
    println("Max: " + vec.max)
  }

  def demo(name: String): Task[Unit] = for {
    _ <- Task(println(s"Running ${name}"))
    globe <- demos(name)
    _ <- savePng(globe, name)
  } yield ()

  def main(args: Array[String]): Unit = {
    val runtime = new DefaultRuntime {}
    if (args.length > 0)
      runtime.unsafeRun(demo(args.head))
    else
      runtime.unsafeRun(Task.collectAll(demos.keys.map(demo)))
  }
}
