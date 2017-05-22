package observatory

import observatory.Visualization.predictTemperature

/**
  * 4th milestone: value-added information
  */
object Manipulation {

  /**
    * @param temperatures Known temperatures
    * @return A function that, given a latitude in [-89, 90] and a longitude in [-180, 179],
    *         returns the predicted temperature at this location
    */
  def makeGrid(temperatures: Iterable[(Location, Double)]): (Int, Int) => Double =
    (lat, lon) => predictTemperature(temperatures, Location(lat, lon))


  /**
    * @param temperaturess Sequence of known temperatures over the years (each element of the collection
    *                      is a collection of pairs of location and temperature)
    * @return A function that, given a latitude and a longitude, returns the average temperature at this location
    */
  def average(temperaturess: Iterable[Iterable[(Location, Double)]]): (Int, Int) => Double = {
    (lat, lon) => {
      val aggregate = temperaturess.aggregate(
        (0.0, 0))(
        (acc, yearData) => (acc, yearData) match {
          case ((t, c), d) => (t + predictTemperature(d, Location(lat, lon)), c + 1)
        },
        (d1, d2) => (d1, d2) match {
          case ((t1, c1), (t2, c2)) => (t1 + t2, c1 + c2)
        })
      aggregate._1 / aggregate._2
    }
  }

  /**
    * @param temperatures Known temperatures
    * @param normals      A grid containing the “normal” temperatures
    * @return A sequence of grids containing the deviations compared to the normal temperatures
    */
  def deviation(temperatures: Iterable[(Location, Double)], normals: (Int, Int) => Double): (Int, Int) => Double = {
    (lat, lon) => {
      makeGrid(temperatures)(lat, lon) - normals(lat, lon)
    }
  }


}

