import java.io.File
import javax.imageio.ImageIO
import FractalGenerator._
import scala.collection.immutable.List

/**
 * Main class, used as a simple fractal generating app
 */
object Main {

  def main(args: Array[String]) {
    val colorMapping = toRGB(0, 4.0, (0, 0.8, 1.8)) _
    val size = new Size(1024, 1024)
    val image = generateImage(Fractals.mandelbrot)(colorMapping, size, 3)
    ImageIO.write(scaleDown(2)(image), "PNG", new File("out.png"))
  }

}
