package observatory

import java.time.LocalDate

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class ExtractionTest extends FunSuite {

  test("locateTemperatures works fine for properly populated input") {
    assert(
      Extraction.locateTemperatures(2015, "/stations-test.csv", "/2015-test.csv") ==
        Seq(
          (LocalDate.of(2015, 7, 11), Location(12.2, -20.01), 26.0),
          (LocalDate.of(2015, 7, 12), Location(12.2, -20.01), 20.444444444444443),
          (LocalDate.of(2015, 11, 13), Location(12.2, -20.01), 13.444444444444446)
        )
    )
  }

  test("avg works fine for properly populated input") {
    val temperatures = Extraction.locateTemperatures(2015, "/stations-test.csv", "/2015-test.csv")

    assert(
      Extraction.locationYearlyAverageRecords(temperatures) ==
        Map(Location(12.2, -20.01) -> 19.962962962962962)
    )
  }
}