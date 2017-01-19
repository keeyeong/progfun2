package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = for {
    i <- arbitrary[Int]
    d <- oneOf(const(empty), genHeap)
  } yield insert(i, d)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("insert1") = forAll { (h: H) =>
    findMin(insert(0, insert(Int.MaxValue, h))) <= 0
  }

  property("insert2") = forAll { (h: H) =>
    findMin(insert(0, insert(Int.MinValue, h))) == Int.MinValue
  }

  property("delete1") = forAll { (h: H) =>
    findMin(deleteMin(insert(0, insert(Int.MinValue, h)))) >= Int.MinValue
  }

  property("meld1") = forAll { (h: H, i: H) =>
    findMin(meld(h, i)) == Math.min(findMin(h), findMin(i))
  }

  property("gen2") = forAll { (h: H) =>
    meld(h, empty) == h
  }

  property("empty1") = forAll { (h: H) =>
    isEmpty(meld(empty, empty))
  }

  property("gen8") = forAll { (h: H) =>
    findMin(deleteMin(insert(Int.MinValue, insert(Int.MinValue, h)))) == Int.MinValue
  }

  property("meld2") = forAll { (h: H) =>
    meld(h, empty) == h
  }

  property("gen10") = forAll { (i: Int, j: Int) =>
    val h = insert(j, insert(i, empty))
    findMin(deleteMin(h)) == Math.max(i, j)
  }

  property("empty2") = forAll { (i: Int, j: Int) =>
    val h = insert(j, insert(i, empty))
    isEmpty(deleteMin(deleteMin(h)))
  }

  property("sorted1") = forAll { (li: List[Int]) =>
    val h = li.foldLeft(empty)((b, a) => insert(a, b))
    def genList(heap: H, results: List[Int]): List[Int] = {
      if (isEmpty(heap)) results
      else
        genList(deleteMin(heap), results ::: List(findMin(heap)))
    }
    genList(h, List()) == li.sorted
  }
}
