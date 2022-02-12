package quickcheck

import org.scalacheck.*
import Arbitrary.*
import Gen.*
import Prop.forAll

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap:
  lazy val genHeap: Gen[H] = for {
    v <- arbitrary[Int]
    h <- oneOf(const(empty), genHeap)
  } yield insert(v, h)

  given Arbitrary[H] = Arbitrary(genHeap)

  property("min1") = forAll { (a: Int) =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("min2") = forAll { (a: Int, b: Int) =>
    val h = insert(b, insert(a, empty))
    val minVal = findMin(h)
    if a <= b then minVal == a else minVal == b
  }

  property("gen1") = forAll { (h: H) =>
    val m = if isEmpty(h) then 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("del_check_empty") = forAll { (a: Int) =>
    isEmpty(deleteMin(insert(a, empty)))
  }

  // Given any heap, get a sorted sequence of elements when continually finding and deleting minima.
  property("ord_lst") = forAll { (h: H) =>
    def ordFromHeapToLst(hp: H): List[Int] = hp match {
      case hp1: H if isEmpty(hp1) => Nil
      case _ => findMin(hp) :: ordFromHeapToLst(deleteMin(hp))
    }
    val checkSorted = ordFromHeapToLst(h)
    checkSorted == checkSorted.sorted
  }

  // Finding a minimum of the melding of any two heaps should return a minimum of one or the other.
  property("min_melded") = forAll { (h1: H, h2: H) =>
    val minOfMelded: Int = findMin(meld(h1, h2))
    minOfMelded == findMin(h1) || minOfMelded == findMin(h2)
  }

  // Take two arbitrary heaps, meld together.
  // Then remove min from 1 and insert into 2, meld the results.
  // Compare two melds by comparing sorted sequences.
  property("delmin1_insert2_meld") = forAll { (h1: H, h2: H) =>

    def ordFromHeapToLst(hp: H): List[Int] = hp match {
      case hp1: H if isEmpty(hp1) => Nil
      case _ => findMin(hp) :: ordFromHeapToLst(deleteMin(hp))
    }

    val melded_orig = meld(h1, h2)
    val delmin1 = deleteMin(h1)
    val min1 = findMin(h1)
    val melded_custom = meld(delmin1, insert(min1, h2))
    
    val lst1 = ordFromHeapToLst(melded_orig)
    val lst2 = ordFromHeapToLst(melded_custom)
    lst1 == lst2
  }

