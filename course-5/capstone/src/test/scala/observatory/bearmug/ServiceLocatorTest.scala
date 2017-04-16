package observatory.bearmug

import java.io.ByteArrayInputStream

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
  }

  test("plainService ignore stationsData well for wrong format") {
    assert(new PlainService().stationsData(buff(",2212,+12.200,-020.010")) == Map.empty)
    assert(new PlainService().stationsData(buff("007026,,+12.200,-020.010")) == Map.empty)
    assert(new PlainService().stationsData(buff("007026,2212,12.200,-020.010")) == Map.empty)
    assert(new PlainService().stationsData(buff("007026,2212,+12,-020.010")) == Map.empty)
    assert(new PlainService().stationsData(buff("007026,2212,+12.,-020.010")) == Map.empty)
    assert(new PlainService().stationsData(buff("007026,2212,,-020.010")) == Map.empty)
    assert(new PlainService().stationsData(buff("007026,2212,+12.200,020.010")) == Map.empty)
    assert(new PlainService().stationsData(buff("007026,2212,+12.200,")) == Map.empty)
    assert(new PlainService().stationsData(buff("007026,2212,+12.200,20")) == Map.empty)
  }

  test("plainService picks temperatures for correct input") {

  }

  test("plainService omit temperatures for wrong input") {

  }
}