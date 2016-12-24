package calculator

import org.junit.runner.RunWith
import org.scalatest.{FunSuite, ShouldMatchers}
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class PolynomialSuite extends FunSuite with ShouldMatchers {

  test("computeDelta works for simple case") {

    val a = 1
    val b = 1
    val c = 1

    val delta = Polynomial.computeDelta(Signal(a), Signal(b), Signal(c))
    assert(delta() == b * b - 4 * a * c)
  }

  test("computeSolutions works for simple case") {
    val a = 2
    val b = -3
    val c = 1
    val delta = Polynomial.computeDelta(Signal(a), Signal(b), Signal(c))

    val solution = Polynomial.computeSolutions(Signal(a), Signal(b), Signal(c), delta)
    assert(solution() == Set(1.0, 0.5))
  }

  test("computeSolutions works for single root") {
    val a = 1
    val b = -2
    val c = 1
    val delta = Polynomial.computeDelta(Signal(a), Signal(b), Signal(c))

    val solution = Polynomial.computeSolutions(Signal(a), Signal(b), Signal(c), delta)
    assert(solution() == Set(1.0))
  }
}
