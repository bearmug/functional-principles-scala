package observatory.bearmug

import com.sksamuel.scrimage.Pixel
import observatory.bearmug.Interpolation.plain
import observatory.{Color, Location}
import org.scalatest.FunSuite
import org.scalatest.Matchers._

import scala.collection.immutable

class InterpolationTest extends FunSuite {

  test("nearest point temperature chosen if distance less than 1.0 km") {
    assert(
      plain.predictTemperature(List((Location(10, 10), 11)), Location(10.0001, 10.00001)) == 11
    )
  }

  test("avg temperature chosen for two avg point") {
    assert(
      plain.predictTemperature(List((Location(10, 10), 0), (Location(12, 12), 20)), Location(11, 11)) === 10.0 +- .05
    )
  }

  test("avg temperature chosen for four avg point") {
    assert(
      plain.predictTemperature(List(
        (Location(10, 10), 0),
        (Location(10, 12), 5),
        (Location(12, 10), 15),
        (Location(12, 12), 20)
      ),
        Location(11, 11)) === 10.0 +- .05
    )
  }

  val temperatures = List(
    (32.0, Color(255, 0, 0)),
    (0.0, Color(0, 255, 255)),
    (60.0, Color(255, 255, 255)),
    (12.0, Color(255, 255, 0)),
    (-50.0, Color(33, 0, 107)),
    (-15.0, Color(0, 0, 255)),
    (-27.0, Color(255, 0, 255)),
    (-60.0, Color(0, 0, 0))
  )

  test("color interpolated to black for very low temperature") {
    assert(plain.interpolateColor(temperatures, -700) == Color(0, 0, 0))
    assert(plain.interpolateColor(temperatures, -60.6) == Color(0, 0, 0))
  }

  test("color interpolated to white for very high temperature") {
    assert(plain.interpolateColor(temperatures, 700) == Color(255, 255, 255))
    assert(plain.interpolateColor(temperatures, 60.5) == Color(255, 255, 255))
  }

  test("color interpolated well for medium values") {
    assert(plain.interpolateColor(temperatures, 33.0) == Color(255, 9, 9))
    assert(plain.interpolateColor(temperatures, 59.0) == Color(255, 246, 246))
    assert(plain.interpolateColor(temperatures, 1.0) == Color(21, 255, 234))
    assert(plain.interpolateColor(temperatures, 11.0) == Color(234, 255, 21))
  }

  test("color interpolated for two temperatures") {
    assert(plain.interpolateColor(List((-1.0, Color(255, 0, 0)), (0.0, Color(0, 0, 255))), -0.5) == Color(128, 0, 128))
  }

  test("color picked from list for matches values") {
    assert(plain.interpolateColor(temperatures, 32.0) == Color(255, 0, 0))
    assert(plain.interpolateColor(temperatures, 60.0) == Color(255, 255, 255))
    assert(plain.interpolateColor(temperatures, -27.0) == Color(255, 0, 255))
    assert(plain.interpolateColor(temperatures, -60.0) == Color(0, 0, 0))
  }

  test("visualize works fine for a set of points") {
    val locations = List(
      (Location(40, 10), -30.0),
      (Location(10, -12), 5.0),
      (Location(12, 60), 15.0),
      (Location(82, -42), 40.0)
    )
    val image = plain.visualize(locations, temperatures)
    assert(image.forall((x, y, pixel: Pixel) => {
      val lon = x - 180
      val lat = 90 - y
      val temp = plain.predictTemperature(locations, Location(lat, lon))
      val color = plain.interpolateColor(temperatures, temp)
      color.red == pixel.red && color.green == pixel.green && color.blue == pixel.blue
    }))
  }

}
