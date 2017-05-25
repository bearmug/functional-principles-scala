package observatory

import com.sksamuel.scrimage.{Image, Pixel}
import observatory.bearmug.Interpolation.plain

/**
  * 5th milestone: value-added information visualization
  */
object Visualization2 {

  /**
    * @param x   X coordinate between 0 and 1
    * @param y   Y coordinate between 0 and 1
    * @param d00 Top-left value
    * @param d01 Bottom-left value
    * @param d10 Top-right value
    * @param d11 Bottom-right value
    * @return A guess of the value at (x, y) based on the four known values, using bilinear interpolation
    *         See https://en.wikipedia.org/wiki/Bilinear_interpolation#Unit_Square
    */
  def bilinearInterpolation(
                             x: Double,
                             y: Double,
                             d00: Double,
                             d01: Double,
                             d10: Double,
                             d11: Double
                           ): Double = {
    d00 * (1 - x) * (1 - y) + d10 * x * (1 - y) + d01 * (1 - x) * y + d11 * x * y
  }

  /**
    * @param grid   Grid to visualize
    * @param colors Color scale to use
    * @param zoom   Zoom level of the tile to visualize
    * @param x      X value of the tile to visualize
    * @param y      Y value of the tile to visualize
    * @return The image of the tile at (x, y, zoom) showing the grid using the given color scale
    */
  def visualizeGrid(
                     grid: (Int, Int) => Double,
                     colors: Iterable[(Double, Color)],
                     zoom: Int,
                     x: Int,
                     y: Int
                   ): Image = {
    val data = {
      for {
        yZoom <- (256 * y) until (256 * (y + 1))
        xZoom <- (256 * x) until (256 * (x + 1))
      } yield plain.tileLocation(zoom + 8, xZoom, yZoom)
    }.map(location => {
      val lowX = location.lon.floor.toInt
      val lowY = location.lat.floor.toInt
      val d00 = grid(lowY, lowX)
      val d01 = grid(lowY + 1, lowX)
      val d10 = grid(lowY, lowX + 1)
      val d11 = grid(lowY + 1, lowX + 1)
      bilinearInterpolation(location.lon - lowX, location.lat - lowY, d00, d01, d10, d11)
    })
      .map(plain.interpolateColor(colors, _))
      .map(c => Pixel(c.red, c.green, c.blue, 127))
      .toArray
    Image(256, 256, data)
  }

}
