import java.awt.RenderingHints
import java.awt.image.BufferedImage
import javax.imageio.ImageIO
import java.io.File

import scala.collection.generic.CanBuildFrom
import scala.collection.immutable.List

object FractalGenerator {

  /**
   * This function convert an arbitrary number n to a color
   * output.Red = (n * contrast + offset) * colorCoeffs(0)
   * output.Green = (n * contrast + offset) * colorCoeffs(1)
   * output.Blue = (n * contrast + offset) * colorCoeffs(2)
   *
   * @param brightness an offset added during the first stage of the conversion
   * @param contrast scaling foactor for the first pass
   * @param colorCoefficient a Tuple of three coefficients resp. for Red, Blue and Green value
   * @param n a number that will be converted to a color
   * @return The color as an int in the ARGB color model
   */
  def toRGB(brightness: Double, contrast: Double, colorCoefficient: (Double, Double, Double))(n: Double): Int = {
    def toValue(coefficient: Double) = 0xFF & ((brightness + contrast * n) * coefficient).toInt
    0xFF << 24 + toValue(colorCoefficient._1) << 16 + toValue(colorCoefficient._2) << 8 + toValue(colorCoefficient._3)
  }


  /**
   * Scale down an image by a factor, using bicubic interpolation
   * @param factor reduction factor, new height will be newHeight = oldHeight / factor
   * @param original Original image
   * @return reduced image
   */
  def scaleDown( factor: Double )( original: BufferedImage ) =
  {
    val newSize = List( original.getWidth, original.getHeight ).map( s => (s / factor).toInt ).toArray
    val resized = new BufferedImage(newSize(0), newSize(1), original.getType)
    val g = resized.createGraphics()
    g.setRenderingHint(RenderingHints.KEY_INTERPOLATION, RenderingHints.VALUE_INTERPOLATION_BICUBIC)
    g.drawImage(original, 0, 0, newSize(0), newSize(1), 0, 0, original.getWidth, original.getHeight, null)
    g.dispose()
    resized
  }


  /**
   * Generate an image of size size and zoom level relativeZoomFactor
   * @param fractal fractal function
   * @param colorMapping color mapping fonction, from the output of fractal to the pixel format
   * @param size size of the image to generate
   * @param relativeZoomFactor zoom factor of the fractal to be generated. It is independant of the output size
   * @return
   */
  def generateImage(fractal: (Double, Double) => Double)
             (colorMapping: (Double)=>Int, size:Size, relativeZoomFactor:Double): BufferedImage = {

    val zoomFactor = size.sum() / (2 * relativeZoomFactor)
    val res = for {
      j <- 0 until size.h
      i <- 0 until size.w
      x0 = (i - size.w/2) / zoomFactor
      y0 = (j - size.h/2) / zoomFactor
    } yield colorMapping(fractal(x0, y0))

    val w = new BufferedImage(size.w, size.h, BufferedImage.TYPE_INT_ARGB)
    w.setRGB(0, 0, size.w, size.h, res.toArray, 0, size.w)
    w
   }


  class Size(width: Int, height: Int) {
    val h = height
    val w = width

    def sum() = h + w
  }

}
