package triggernz.weather.image

import java.awt.geom.AffineTransform
import java.awt.image.{AffineTransformOp, BufferedImage}

import javax.imageio.ImageIO
import triggernz.weather.Globe

/**
  * Functions around image IO and conversions between doubles and image bytes
  */
object Image {

  def scaleDown(before: BufferedImage, factor: Int) = {
    val w= before.getWidth()
    val h = before.getHeight()

    val after = new BufferedImage(w/factor, h/factor, BufferedImage.TYPE_BYTE_GRAY)
    val at = new AffineTransform()
    val factorInv = 1.0 / factor
    at.scale(factorInv, factorInv)
    val scaleOp = new AffineTransformOp(at, AffineTransformOp.TYPE_BILINEAR)
    scaleOp.filter(before, after)
    after
  }

  def load(resourcePath: String): BufferedImage = {
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
