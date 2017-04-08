package example

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{BeforeAndAfterAll, FunSuite}
import Example._
import org.apache.spark.UtilReflection
import org.junit.Ignore

@Ignore
@RunWith(classOf[JUnitRunner])
class ExampleSuite extends FunSuite with BeforeAndAfterAll {

  def initializeExample(): Boolean =
    try {
      System.setProperty("SPARK_LOCAL_IP", "localhost")
      Example
      true
    } catch {
      case _: Throwable => false
    }

  override def afterAll(): Unit = {
    assert(initializeExample(), " -- did you fill in all the values in Example (conf, sc)?")
    sc.stop()
  }

  def reflector(ref: AnyRef) = new {
    def getV(name: String): Any = ref.getClass.getMethods.find(_.getName == name).get.invoke(ref)
    def setV(name: String, value: Any): Unit = ref.getClass.getMethods.find(_.getName == name).get.invoke(ref, value.asInstanceOf[AnyRef])
  }

//  lazy val localIpAddress: String = findLocalIpAddress()
//  lazy val localIpAddressHostname: String = getAddressHostName(localIpAddress)

  test("'sumOfPlusOnes List(1, 2, 3, 4, 5)' should be equal to 20") {
    assert(initializeExample(), " -- did you fill in all the values in Example (conf, sc)?")
    UtilReflection.amend("localIpAddress", "127.0.0.1")
    UtilReflection.amend("localIpAddressHostname", "localhost")
    assert(sumOfPlusOnes == 20)
  }
}

