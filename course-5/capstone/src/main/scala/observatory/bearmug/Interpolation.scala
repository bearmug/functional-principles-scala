package observatory.bearmug

import com.sksamuel.scrimage.{Image, Pixel}
import observatory.{Color, Location}

import scala.annotation.tailrec
import scala.math._

trait Interpolation {
  def predictTemperature(temperatures: Iterable[(Location, Double)], location: Location): Double

  def interpolateColor(points: Iterable[(Double, Color)], value: Double): Color

  def visualize(temperatures: Iterable[(Location, Double)], colors: Iterable[(Double, Color)]): Image

  def tileLocation(zoom: Int, x: Int, y: Int): Location

  def tile(temperatures: Iterable[(Location, Double)], colors: Iterable[(Double, Color)], zoom: Int, x: Int, y: Int): Image

  def generateTiles[Data](
                           yearlyData: Iterable[(Int, Data)],
                           generateImage: (Int, Int, Int, Int, Data) => Unit
                         ): Unit
}

object Interpolation {

  val R = 6372.8

  def distance(l1: Location, l2: Location): Double =
    R * acos(sin(l1.lat.toRadians) * sin(l2.lat.toRadians) +
      cos(l1.lat.toRadians) * cos(l2.lat.toRadians) * cos(abs(l1.lon - l2.lon).toRadians))

  private object PlainInterpolation extends Interpolation {

    val P = 2

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
        .map(c => Pixel(c.red, c.green, c.blue, 127))
        .toArray

      Image(360, 180, data)
    }

    override def tileLocation(zoom: Int, x: Int, y: Int): Location = {
      val base = Math.pow(2.0, zoom)
      val longitude = x / base * 360.0 - 180.0
      val latitudeRad = math.atan(math.sinh(math.Pi * (1 - 2 * y / base)))
      val latitude = math.toDegrees(latitudeRad)
      Location(latitude, longitude)
    }

    override def tile(temperatures: Iterable[(Location, Double)], colors: Iterable[(Double, Color)], zoom: Int, x: Int, y: Int): Image = {
      val data = {
        for {
          yZoom <- (256 * y) until (256 * (y + 1))
          xZoom <- (256 * x) until (256 * (x + 1))
        } yield tileLocation(zoom + 8, xZoom, yZoom)
      }.map(predictTemperature(temperatures, _))
        .map(interpolateColor(colors, _))
        .map(c => Pixel(c.red, c.green, c.blue, 127))
        .toArray
      Image(256, 256, data)
    }

    override def generateTiles[Data](yearlyData: Iterable[(Int, Data)], generateImage: (Int, Int, Int, Int, Data) => Unit): Unit =
      yearlyData.foreach {
        case (year, data) => for {
          zoomLevel <- 0 to 3
          x <- 0 until Math.pow(2, zoomLevel).toInt
          y <- 0 until Math.pow(2, zoomLevel).toInt
        } yield generateImage(year, zoomLevel, x, y, data)
      }
  }

  def plain: Interpolation = PlainInterpolation
}
