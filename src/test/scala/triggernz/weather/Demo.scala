package triggernz.weather

import java.awt.{Image => JImage}
import java.awt.image.BufferedImage
import triggernz.weather.image.Image

import scalaz.zio._

import javax.swing.{ImageIcon, JLabel, JOptionPane}
import triggernz.weather.image._

object Demo {
  val elevations: Task[Globe[Byte]] =
    timeTask("Mapping image to globe", Image.load("gebco_08_rev_elev_21600x10800.png").map(Image.imageToGlobe))

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

  def displayGlobe(g: Globe[Byte]): Task[Unit] =
    for {
      fromGlobe <- globeToImage(g)
      scaled <- scaleDown(fromGlobe)
      _ <- displayRaw(scaled)
    } yield ()

  def scaleDown(bi: BufferedImage): Task[JImage] =
    timeTask("Scaling", Task(bi.getScaledInstance(1080, 540, 0)))

  def displayRaw(img: JImage): Task[Unit] = Task {
    JOptionPane.showMessageDialog(null, new JLabel(new ImageIcon(img)))
  }


  def showMinMax(globe: Globe[Byte]): Task[Unit] = Task {
    val vec = globe.toFlatArray
    println("Min: " + vec.min)
    println("Max: " + vec.max)
  }

  def demo: Task[Unit] = for {
    globe <- elevations
    _ <- showMinMax(globe)
    _ <- displayGlobe(globe)
  } yield ()

  def main(args: Array[String]): Unit = {
    val runtime = new DefaultRuntime {}
    runtime.unsafeRun(demo)
  }
}
