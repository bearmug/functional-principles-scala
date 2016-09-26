package quickcheck

import common._
import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = for {
    a <- Arbitrary.arbitrary[Int]
    h <- oneOf(const(empty), genHeap)
  } yield insert(a, h)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("deleteMin") = forAll { (h: H) =>
    val m1 = if (isEmpty(h)) 0 else findMin(h)
    val h1 = meld(h, h)
    val m2 = if (isEmpty(h1)) 0 else findMin(h1)

    m1 == m2
  }

  property("meldNil") = forAll { (h: H) =>
    meld(h, empty) == h
  }
}
