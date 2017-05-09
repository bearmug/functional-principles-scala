package observatory.bearmug

import com.sksamuel.scrimage.{Image, Pixel}
import observatory.{Color, Location}

import scala.annotation.tailrec
import scala.math._

trait Interpolation {
  def predictTemperature(temperatures: Iterable[(Location, Double)], location: Location): Double

  def interpolateColor(points: Iterable[(Double, Color)], value: Double): Color

  def visualize(temperatures: Iterable[(Location, Double)], colors: Iterable[(Double, Color)]): Image
}

object Interpolation {

  private object PlainInterpolation extends Interpolation {

    val R = 6372.8
    val P = 2

    def distance(loc1: Location, loc2: Location): Double =
      R * acos(sin(loc1.lat) * sin(loc2.lat) + cos(loc1.lat) * cos(loc2.lat) * cos(abs(loc1.lon - loc2.lon)))

    def interpolate(temperatures: Iterable[(Location, Double)], location: Location): Double =
      temperatures.foldLeft((0.0, 0.0))((acc, loc) => {
        val w = 1 / math.pow(distance(loc._1, location), P)
        (acc._1 + w * loc._2, acc._2 + w)
      }) match {
        case (weightedSum, weights) => weightedSum / weights
      }

    override def predictTemperature(temperatures: Iterable[(Location, Double)], location: Location): Double = {
      temperatures.find(t => distance(t._1, location) < 1.0) match {
        case Some((_, temp)) => temp
        case None => interpolate(temperatures, location)
      }
    }

    override def interpolateColor(points: Iterable[(Double, Color)], value: Double): Color = {
      @tailrec
      def interpolate(left: Option[(Double, Color)], pts: Iterable[(Double, Color)]): Color = (left, pts) match {
        case (None, Nil) => throw new IllegalStateException(s"nothing interpolate for $value")
        case (_, (t, c) :: _) if value == t => c
        case (None, (t, c) :: _) if value < t => c
        case (None, (t, c) :: tail) => interpolate(Some((t, c)), tail)
        case (Some((_, c)), Nil) => c
        case (Some((_, _)), (t2, c2) :: tail) if value > t2 => interpolate(Some((t2, c2)), tail)
        case (Some((t1, Color(r1, g1, b1))), (t2, Color(r2, g2, b2)) :: _) => {
          val ratio: Double = (value - t1) / (t2 - t1)
          Color(
            (r1 + (r2 - r1) * ratio).round.toInt,
            (g1 + (g2 - g1) * ratio).round.toInt,
            (b1 + (b2 - b1) * ratio).round.toInt
          )
        }
      }

      interpolate(None, points.toList.sortBy(_._1))
    }

    override def visualize(temperatures: Iterable[(Location, Double)], colors: Iterable[(Double, Color)]): Image = {
      val data = {
        for {
          lat <- (-89 to 90 reverse)
          lon <- -180 to 179
        } yield predictTemperature(temperatures, Location(lat, lon))
      }.map(interpolateColor(colors, _))
        .map(c => Pixel(c.red, c.green, c.blue, 255))
        .toArray

      Image(360, 180, data)
    }
  }

  def plain: Interpolation = PlainInterpolation
}
