
import common._

package object scalashop {

  /** The value of every pixel is represented as a 32 bit integer. */
  type RGBA = Int

  /** Returns the red component. */
  def red(c: RGBA): Int = (0xff000000 & c) >>> 24

  /** Returns the green component. */
  def green(c: RGBA): Int = (0x00ff0000 & c) >>> 16

  /** Returns the blue component. */
  def blue(c: RGBA): Int = (0x0000ff00 & c) >>> 8

  /** Returns the alpha component. */
  def alpha(c: RGBA): Int = (0x000000ff & c) >>> 0

  /** Used to create an RGBA value from separate components. */
  def rgba(r: Int, g: Int, b: Int, a: Int): RGBA = {
    (r << 24) | (g << 16) | (b << 8) | (a << 0)
  }

  /** Restricts the integer into the specified range. */
  def clamp(v: Int, min: Int, max: Int): Int = {
    if (v < min) min
    else if (v > max) max
    else v
  }

  /** Image is a two-dimensional matrix of pixel values. */
  class Img(val width: Int, val height: Int, private val data: Array[RGBA]) {
    def this(w: Int, h: Int) = this(w, h, new Array(w * h))
    def apply(x: Int, y: Int): RGBA = data(y * width + x)
    def update(x: Int, y: Int, c: RGBA): Unit = data(y * width + x) = c
  }

  /** Computes the blurred RGBA value of a single pixel of the input image. */
  def boxBlurKernel(src: Img, x: Int, y: Int, radius: Int): RGBA = {
    // TODO implement using while loops
    val minX = clamp(x - radius, 0, x)
    val maxX = clamp(x + radius, x, src.width - 1)

    val minY = clamp(y - radius, 0, y)
    val maxY = clamp(y + radius, y, src.height - 1)

    var pixels = 0
    var red = 0
    var green = 0
    var blue = 0
    var alpha = 0

    var currentX = minX
    while (currentX <= maxX) {

      var currentY = minY
      while (currentY <= maxY) {

        val rgba = src(currentX, currentY)
        red += this.red(rgba)
        green += this.green(rgba)
        blue += this.blue(rgba)
        alpha += this.alpha(rgba)

        pixels += 1
        currentY += 1
      }
      currentX += 1
    }

    rgba(red / pixels, green / pixels, blue / pixels, alpha / pixels)
  }

}
