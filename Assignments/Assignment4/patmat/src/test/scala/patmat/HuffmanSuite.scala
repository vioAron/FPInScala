package patmat

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import patmat.Huffman._

@RunWith(classOf[JUnitRunner])
class HuffmanSuite extends FunSuite {
  trait TestTrees {
    val t1 = Fork(Leaf('a', 2), Leaf('b', 3), List('a', 'b'), 5)
    val t2 = Fork(Fork(Leaf('a', 2), Leaf('b', 3), List('a', 'b'), 5), Leaf('d', 4), List('a', 'b', 'd'), 9)
    val myABCTree = Fork(Leaf('a', 3), Fork(Leaf('b', 2), Leaf('c', 1), List('b', 'c'), 3), List('a', 'b', 'c'), 6)
  }

  test("weight of a larger tree") {
    new TestTrees {
      assert(weight(t1) === 5)
    }
  }

  test("chars of a larger tree") {
    new TestTrees {
      assert(chars(t2) === List('a', 'b', 'd'))
    }
  }

  test("string2chars(\"hello, world\")") {
    assert(string2Chars("hello, world") === List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }

  test("counter") {
    assert(times(string2Chars("abcaab")) === List(('c', 1), ('b', 2), ('a', 3)))
  }
  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 3)))
  }

  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leaflist) === List(Fork(Leaf('e', 1), Leaf('t', 2), List('e', 't'), 3), Leaf('x', 4)))
  }

  test("combine of a singleton or nil") {
    assert(combine(List(Leaf('a', 1))) === List(Leaf('a', 1)))
  }

  test("decoding the secret") {
    assert(decodedSecret === List('h', 'u', 'f', 'f', 'm', 'a', 'n', 'e', 's', 't', 'c', 'o', 'o', 'l'))
  }

  test("decoding myABCTree") {
    new TestTrees {
      assert(decode(myABCTree, List(1, 0, 1, 0, 0, 0, 0, 1, 1)) === List('b', 'b', 'a', 'a', 'a', 'c'))
    }
  }

  test("encoding myABCTree") {
    new TestTrees {
      assert(encode(myABCTree)(List('b', 'b', 'a', 'a', 'a', 'c')) === List(1, 0, 1, 0, 0, 0, 0, 1, 1))
    }
  }

  test("decode and encode a very short text should be identity") {
    new TestTrees {
      val bits = encode(t1)("ab".toList)
      assert(decode(t1, bits) === "ab".toList)
    }
  }

  test("my code tree") {
    val sampleTree = makeCodeTree(makeCodeTree(Leaf('x', 1), Leaf('e', 1)), Leaf('t', 2))
  }
}
