package quickcheck

import org.scalacheck.Arbitrary._
import org.scalacheck.Gen._
import org.scalacheck.Prop._
import org.scalacheck._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = for {
    a <- Arbitrary.arbitrary[Int]
    h <- oneOf(const(empty), genHeap)
  } yield insert(a, h)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("findMin") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("deleteMin") = forAll { (h: H) =>
    def length(h: H): Int = h match {
      case hs if isEmpty(hs) => 0
      case hs => 1 + length(deleteMin(hs))
    }

    if (length(h) > 0 && findMin(h) > Integer.MIN_VALUE) {
      val m = findMin(h)
      findMin(deleteMin(insert(m, insert(m - 1, h)))) == m
    } else true
  }

  property("meldNil") = forAll { (h: H) =>
    meld(h, empty) == h
  }
}
