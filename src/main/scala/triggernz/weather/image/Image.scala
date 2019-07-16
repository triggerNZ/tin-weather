package triggernz.weather.image

import java.awt.image.{BufferedImage, DataBufferByte}

import javax.imageio.ImageIO
import triggernz.weather.Globe
import scalaz.zio.Task

object Image {
  def load(resourcePath: String): Task[BufferedImage] = Task {
    val stream = this.getClass().getClassLoader().getResourceAsStream(resourcePath)
    ImageIO.read(stream)
  }


  def imageToGlobe(bi: BufferedImage): Globe[Byte] = {
    val intBuf = Array.ofDim[Int](bi.getWidth() * bi.getHeight())
    val bytes = bi.getRaster.getPixels(0, 0, bi.getWidth(), bi.getHeight(), intBuf).map(i => (i & 0xFF).toByte)
    Globe.fromArray(bytes, bi.getHeight, bi.getWidth())
  }
  def globeToImage(g: Globe[Byte]): BufferedImage = {
    val img = new BufferedImage(g.lngCount, g.latCount, BufferedImage.TYPE_BYTE_GRAY )
    img.getRaster.setDataElements(0, 0, g.lngCount, g.latCount, g.toFlatArray)
    img
  }
}
