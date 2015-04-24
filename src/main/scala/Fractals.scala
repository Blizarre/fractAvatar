object Fractals {

  /**
    * Mandelbrot Fractal, see @link:http://en.wikipedia.org/wiki/Mandelbrot_set
    * @param x0 x position in the plane
    * @param y0 y position in the plane
    * @return the number of iterations needed to determine if the point (x0, y0) is in the Mandelbrot Set
    */
  def mandelbrot(x0: Double, y0: Double) = {
    def inner(x:Double, y:Double, nbIter: Int):Double = {
      if (nbIter > 1000) nbIter
      else if (x * x + y * y > 4) nbIter
      else inner(x * x - y * y + x0, 2 * x * y + y0, nbIter + 1)
    }
    inner(0, 0, 0)
  }

  /**
   * Julia Fractal, see @link:http://en.wikipedia.org/wiki/Julia_set
   * @param c1 complex parameter (Real value)
   * @param c2 complex parameter (Imaginary value)
   * @param x0 x position in the plane
   * @param y0 y position in the plane
   * @return the number of iterations needed to determine if the point (x0, y0) is in the Julia Set for the given complex (c1 + i.c2)
   */
  def julia(c1: Double, c2: Double)(x0: Double, y0: Double) = {
    def inner(x:Double, y:Double, nbIter: Int):Double = {
      if (nbIter > 1000) nbIter
      else if (x * x + y * y > 4) nbIter
      else inner(x * x - y * y + c1, 2 * x * y + c2, nbIter + 1)
    }
    inner(x0, y0, 0)
  }

}
