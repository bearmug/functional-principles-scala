package observatory.bearmug

import observatory.Location
import observatory.bearmug.Interpolation.plain
import org.scalatest.FunSuite

import org.scalatest._
import org.scalatest.Matchers._

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

}
