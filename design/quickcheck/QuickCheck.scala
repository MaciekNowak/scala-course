package quickcheck

import java.util.NoSuchElementException

import common._
import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = oneOf(const(empty),
    for {i: Int <- arbitrary[Int]
         j: Int = if (i >= 0) i else 0 - i
         h: H <- oneOf(const(empty), genHeap, genHeap, genHeap)
    } yield insert(j, h))

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("two1") = forAll { (a: Int, b: Int) =>
    val h = insert(a, empty)
    val nh = insert(b, h)
    findMin(nh) == scala.math.min(a, b)
  }

  property("del1") = forAll { (a: Int) =>
    val h = deleteMin(insert(a, empty))
    isEmpty(h)
  }

  property("del2") = forAll { (h: H) =>
    def walk(hh: H): List[Int] = {
      if(isEmpty(hh: H)) List()
      else {
        val m = findMin(hh)
        m :: walk(deleteMin(hh))
      }
    }
    val l = walk(h)
    val s = l.sorted

    l == s
  }

  property("meld1") = forAll { (h1: H, h2: H) =>
    var oki = true
    val h = meld(h1, h2)

    def isEmptyWorks(hh: H): Boolean = {
      var itDoes = true
      var minVal = -1
      val e = isEmpty(hh)
      for (i <- 0 to 5) {
        if (itDoes)
          if (e == isEmpty(hh)) {
            if (!e)
              try {
                val m = findMin(hh)
                if (minVal < 0) minVal = m
                if (minVal != m) itDoes = false
              } catch {
                case e: NoSuchElementException => itDoes = false
              }
          }
          else
            itDoes = false
      }
      if (!itDoes) println("isEmptyWorks failed")
      itDoes
    }

    oki = isEmptyWorks(h) && isEmptyWorks(h1) && isEmptyWorks(h2)

    if (oki) {
      if (isEmpty(h1) && isEmpty(h2)) {
        if (isEmpty(h))
          oki = true
        else
          oki = false
      } else if (isEmpty(h)) {
        oki = false
      } else {
        val m = findMin(h)

        if (isEmpty(h1) && !isEmpty(h2)) {
          oki = findMin(h2) == m
        } else if (!isEmpty(h1) && isEmpty(h2)) {
          oki = findMin(h1) == m
        } else {
          val min = if(findMin(h1) > findMin(h2)) findMin(h2) else findMin(h1)
          oki = min == m
        }
      }
    }
    oki
  }

  property("meld2") = forAll { (h1: H, h2: H) =>
    var oki = true
    val h = meld(h1, h2)

    def walk(hh: H): List[Int] = {
      if (isEmpty(hh: H)) List()
      else {
        val m = findMin(hh)
        m :: walk(deleteMin(hh))
      }
    }

    if (!isEmpty(h1) || !isEmpty(h2)) {
      if (isEmpty(h)) {
        oki = false
      } else {
        val l = walk(h)
        val s = l.sorted
        if (l == s) {
          val l1 = walk(h1)
          val l2 = walk(h2)
          var ll: List[Int] = List()
          if (!l1.isEmpty) ll = l1
          if (!l2.isEmpty) ll = l2 ::: ll
          val ss = ll.sorted
          if (s != ss) oki = false
        } else
          oki = false
      }
    }
    oki
  }
}

