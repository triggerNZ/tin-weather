package triggernz.weather.image

import java.awt.image.{BufferedImage, DataBufferByte}

import javax.imageio.ImageIO
import triggernz.weather.Globe

object Image {

  def loadUnsafe(resourcePath: String): BufferedImage = {
    val stream = this.getClass().getClassLoader().getResourceAsStream(resourcePath)
    ImageIO.read(stream)
  }

  def percentToByte(percentage: Double): Byte = ((percentage * 255).toInt & 0xFF).toByte
  def byteToPercent(b: Byte): Double = (b.toInt & 0xFF) / 255.0


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
