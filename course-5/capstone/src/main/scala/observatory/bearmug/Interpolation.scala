package observatory.bearmug

import observatory.Location

import scala.math._

trait Interpolation {
  def predictTemperature(temperatures: Iterable[(Location, Double)], location: Location): Double
}

object Interpolation {
  private object PlainInterpolation extends Interpolation {

    val R = 6372.8

    def distance(loc1: Location, loc2: Location): Double =
      R * acos(sin(loc1.lat)*sin(loc2.lat)+cos(loc1.lat)*cos(loc2.lat)*cos(abs(loc1.lon - loc2.lon)))

    def interpolate(temperatures: Iterable[(Location, Double)], location: Location): Double = ???

    override def predictTemperature(temperatures: Iterable[(Location, Double)], location: Location): Double = {
      temperatures.find(t => distance(t._1, location) < 1.0) match {
        case Some((_, temp)) => temp
        case None => interpolate(temperatures, location)
      }
    }
  }

  def plain: Interpolation = PlainInterpolation
}
