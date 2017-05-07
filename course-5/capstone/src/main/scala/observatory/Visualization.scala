package observatory

import com.sksamuel.scrimage.{Image, Pixel}
import observatory.bearmug.Interpolation.plain

/**
  * 2nd milestone: basic visualization
  */
object Visualization {

  /**
    * @param temperatures Known temperatures: pairs containing a location and the temperature at this location
    * @param location     Location where to predict the temperature
    * @return The predicted temperature at `location`
    */
  def predictTemperature(temperatures: Iterable[(Location, Double)], location: Location): Double = {
    plain.predictTemperature(temperatures, location)
  }

  /**
    * @param points Pairs containing a value and its associated color
    * @param value  The value to interpolate
    * @return The color that corresponds to `value`, according to the color scale defined by `points`
    */
  def interpolateColor(points: Iterable[(Double, Color)], value: Double): Color = {
    plain.interpolateColor(points, value)
  }

  /**
    * @param temperatures Known temperatures
    * @param colors       Color scale
    * @return A 360×180 image where each pixel shows the predicted temperature at its location
    */
  def visualize(temperatures: Iterable[(Location, Double)], colors: Iterable[(Double, Color)]): Image = {
    val data = {
      for {
        lon <- 180 to 179
        lat <- 90 to -89
      } yield predictTemperature(temperatures, Location(lat, lon))
    }
      .map(interpolateColor(colors, _))
      .map(c => Pixel(c.red, c.green, c.blue, 255))
      .toArray

    Image(360, 180, data)
  }

}

