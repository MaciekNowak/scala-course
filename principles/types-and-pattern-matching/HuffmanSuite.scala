package patmat

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import patmat.Huffman._

@RunWith(classOf[JUnitRunner])
class HuffmanSuite extends FunSuite {
	trait TestTrees {
	  val t0 = Leaf('b',3)
		val t1 = Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5)
		val t2 = Fork(Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5), Leaf('d',4), List('a','b','d'), 9)
		val t3 = Fork(Leaf('a',8), Fork(Fork(Leaf('b', 3), Fork(Leaf('c', 1), Leaf('d', 1), List('c','d'), 2), List('b','c','d'), 5), Fork(Fork(Leaf('e', 1), Leaf('f', 1), List('e','f'), 2), Fork(Leaf('g', 1), Leaf('h', 1), List('g','h'), 2), List('e','f','g','h'), 4), List('b','c','d','e','f','g','h'), 9), List('a','b','c','d','e','f','g','h'), 17)
	}


  test("weight of a larger tree") {
    new TestTrees {
      assert(weight(t0) === 3)
      assert(weight(t1) === 5)
      assert(weight(t2) === 9)
      assert(weight(t3) === 17)
    }
  }


  test("chars of a larger tree") {
    new TestTrees {
      assert(chars(t0) === List('b'))
      assert(chars(t1) === List('a','b'))
      assert(chars(t2) === List('a','b','d'))
      assert(chars(t3) === List('a','b','c','d','e','f','g','h'))
    }
  }


  test("string2chars(\"hello, world\")") {
    assert(string2Chars("hello, world") === List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }

  test("times()") {
    assert(times(List('a')) === List(('a', 1)))
    assert(times(List('a','b','c')) === List(('c', 1), ('b', 1), ('a', 1)))
    assert(times(List('a','a','a')) === List(('a', 3)))
    assert(times(List('a','b','c','a','a','d','c')) === List(('c', 2), ('d', 1), ('a', 3), ('b', 1)))
  }
  
  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e',1), Leaf('t',2), Leaf('x',3)))
  }

  test("singleton") {
    new TestTrees {
      assert(singleton(List()) === false)
      assert(singleton(List(t1)) === true)
      assert(singleton(List(t1, t2)) === false)
    }
  }
  
  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leaflist) === List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4)))
  }
  
  test("until") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(until(singleton, combine)(leaflist) === List(Fork(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4), List('e','t','x'), 7)))
  }

  test("decode") {
    new TestTrees {
      assert(decode(t3, List(1,0,1,1)) === "d".toList)
      assert(decode(t3, List(1,0,0,0,1,0,1,0)) === "bac".toList)
    }
  }
 
  test("decodedSecret") {
    assert(decodedSecret === "huffmanestcool".toList)
  }
    
  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
      assert(decode(t3, encode(t3)("d".toList)) === "d".toList)
      assert(decode(t3, encode(t3)("bac".toList)) === "bac".toList)
    }
  }

  test("codeBits") {
    val t: CodeTable = List(('a', List(1, 0, 1)), ('b', List(1, 1, 1)))
    assert(codeBits(t)('x') === List())
    assert(codeBits(t)('a') === List(1, 0, 1))
    assert(codeBits(t)('b') === List(1, 1, 1))
  }
  
  test("convert") {
    new TestTrees {
      assert(convert(t1) === List(('a', List(0)), ('b', List(1))))
      assert(convert(t3) === List(('a', List(0)),
                                  ('b', List(1, 0, 0)),
                                  ('c', List(1, 0, 1, 0)),
                                  ('d', List(1, 0, 1, 1)),
                                  ('e', List(1, 1, 0, 0)),
                                  ('f', List(1, 1, 0, 1)),
                                  ('g', List(1, 1, 1, 0)),
                                  ('h', List(1, 1, 1, 1))
                                 )
            )
    }
  }
  
  test("quickEncode") {
    new TestTrees {
      assert(quickEncode(t1)("ab".toList) === List(0, 1))
      assert(quickEncode(t1)("abba".toList) === List(0, 1, 1, 0))
      assert(quickEncode(t3)("abba".toList) === List(0, 1, 0, 0, 1, 0, 0, 0))
      assert(quickEncode(frenchCode)("huffmanestcool".toList) === List(0,0,1,1,1,0,1,0,1,1,1,0,0,1,1,0,1,0,0,1,1,0,1,0,1,1,0,0,1,1,1,1,1,0,1,0,1,1,0,0,0,0,1,0,1,1,1,0,0,1,0,0,1,0,0,0,1,0,0,0,1,0,1))
      assert(decode(frenchCode, quickEncode(frenchCode)("huffmanestcool".toList)) === "huffmanestcool".toList)
    }
  }
}
