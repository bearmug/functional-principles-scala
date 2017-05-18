package observatory.bearmug

import java.io.ByteArrayInputStream
import java.time.LocalDate

import observatory.Location
import observatory.bearmug.ServiceLocator.PlainService
import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

import scala.io.BufferedSource

@RunWith(classOf[JUnitRunner])
class ServiceLocatorTest extends FunSuite {

  def buff(s: String): BufferedSource = new BufferedSource(new ByteArrayInputStream(s.getBytes))

  test("plainService picks stationsData well for proper format") {
    assert(
      new PlainService().stationsData(buff("007026,2212,+12.200,-020.010")) ==
        Map("007026:2212" -> Location(+12.200, -020.010))
    )

    assert(
      new PlainService().stationsData(buff(
        """007026,2212,+12.200,-020.010
          |1,2,+1.200,-01.010""".stripMargin)) ==
        Map(
          "007026:2212" -> Location(12.2, -20.01),
          "1:2" -> Location(1.2, -1.01))
    )
  }

  test("plainService ignore stationsData well for wrong format") {
    assert(new PlainService().stationsData(buff("007026,2212,12.200,-020.010")) == Map.empty)
    assert(new PlainService().stationsData(buff("007026,2212,+12,-020.010")) == Map.empty)
    assert(new PlainService().stationsData(buff("007026,2212,+12.,-020.010")) == Map.empty)
    assert(new PlainService().stationsData(buff("007026,2212,,-020.010")) == Map.empty)
    assert(new PlainService().stationsData(buff("007026,2212,+12.200,020.010")) == Map.empty)
    assert(new PlainService().stationsData(buff("007026,2212,+12.200,")) == Map.empty)
    assert(new PlainService().stationsData(buff("007026,2212,+12.200,20")) == Map.empty)
  }

  test("plainService picks temperatures for correct input") {
    assert(new PlainService().itr(
      2015,
      "stations-source-file",
      buff(
        """007026,2212,+12.200,-020.010
          |1,2,+1.200,-01.010""".stripMargin),
      buff(
        """007026,2212,07,11,+78.8
          |007026,2212,07,12,68.8
          |007026,2212,11,13,-56.2""".stripMargin)) ==
      Seq(
        (LocalDate.of(2015, 7, 11), Location(12.2, -20.01), 26.0),
        (LocalDate.of(2015, 7, 12), Location(12.2, -20.01), 20.444444444444443),
        (LocalDate.of(2015, 11, 13), Location(12.2, -20.01), -49.0)
      )
    )
  }

  test("plainService omit temperatures for wrong input") {
    assert(new PlainService().itr(2015, "stations-source-file",
      buff("007026,2212,+12.200,-020.010"),
      buff(
        """007026,2212,,12,68.8
          |007026,2212,07,,68.8
          |007026,2212,07,12,
          |007026,2212,07,12,.68""".stripMargin)) ==
      Seq.empty
    )
  }

  test("plainService calc avg temperatures for correct input") {
    val service = new PlainService()
    assert(service.yar(service.itr(
      2015,
      "stations-source-file",
      buff(
        """007026,2212,+12.200,-020.010
          |1,2,+1.200,-01.010""".stripMargin),
      buff(
        """007026,2212,07,11,+78.8
          |007026,2212,07,12,8.8
          |007026,2212,11,13,-56.2""".stripMargin))) ==
      Map(
        (Location(12.2,-20.01), -11.962962962962962)
      )
    )
  }
}