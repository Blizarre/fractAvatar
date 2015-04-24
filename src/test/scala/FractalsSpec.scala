import org.scalatest._

/**
 * Test class for fractals. Right now this is quite ridiculous, with should be < X when X depends on the bailout
 * term hardcoded in the fractal. More work needed to test it thoroughly
 */
class FractalsSpec extends FlatSpec with Matchers {

  "The mandelbrot function" should "return a large number for points right inside the set" in {
    Fractals.mandelbrot(0, 0) should be > 100.0
    Fractals.mandelbrot(-0.5, 0) should be > 100.0
  }

  "The mandelbrot function" should "return a small number for points far outside the set" in {
    Fractals.mandelbrot(2, 20) should be < 3.0
    Fractals.mandelbrot(-5, 0) should be < 3.0
    Fractals.mandelbrot(-5, 15) should be < 3.0
    Fractals.mandelbrot(5, -3) should be < 3.0
  }
}