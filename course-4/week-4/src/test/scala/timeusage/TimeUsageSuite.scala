package timeusage

import org.apache.spark.sql.Column
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{BeforeAndAfterAll, FunSuite}

@RunWith(classOf[JUnitRunner])
class TimeUsageSuite extends FunSuite with BeforeAndAfterAll {

  test("classifiedColumns works just fine") {
    val workCol = Set("t05", "t180544", "t053a")
    val primCol = Set("t01", "t0345", "t11", "t1801aaa", "t1803")
    val lesrCol = Set("t0200000", "t0444")
    val alenCol = Set("alienCol")

    val (p, w, l) = TimeUsage.classifiedColumns((Set.empty ++ workCol ++ primCol ++ lesrCol ++ alenCol).toList)
    assert(p.toSet == primCol.map(new Column(_)))
    assert(w.toSet == workCol.map(new Column(_)))
    assert(l.toSet == lesrCol.map(new Column(_)))
  }
}
