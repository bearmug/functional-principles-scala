package observatory.bearmug

import observatory.Location
import org.scalatest.FunSuite

class InterpolationTest extends FunSuite {

  test("nearest point temperature chosen if distance less than 1.0 km") {
    val interpolation: Interpolation = observatory.bearmug.Interpolation.plain
    assert(interpolation.predictTemperature(List((Location(10, 10), 11)), Location(10.0001, 10.00001)) == 11)
  }

}
