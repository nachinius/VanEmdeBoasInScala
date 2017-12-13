package com.nachinius.vanEmdeBoas

import org.scalatest.{FreeSpec, Matchers}

import scala.util.Random

class vEBTest extends FreeSpec with Matchers {

  "Methods tests" - {
    "insert" in {
      val a = vanEmdeBoas(8)
      assert(a.insert(3).member(3))
      assert(a.insert(4).member(3))
      assert(a.member(4))
      val i = (1 << 7) - 1
      assert(a.insert(i).member( i))
    }
    "successor" in {
      val veb = vanEmdeBoas(8)
      val seed = 1
      val rnd = new Random(seed)
      val original = List(2,5,8,12,16,20,24,30,31,32,33,50,54)
      val lst = rnd.shuffle(original)
      // test
      lst foreach veb.insert
      val results = original map veb.successor
      val expectedSuccessor = original.tail.map(Some(_)) :+ None

      veb.successor(2) shouldBe Some(5)
      veb.successor(5) shouldBe Some(8)
      veb.successor(66) shouldBe  None
      veb.successor(0) shouldBe Some(2)
      veb.successor(21) shouldBe Some(24)

      results shouldBe expectedSuccessor

      (original map veb.member) shouldBe original.map(_=>true)
    }
    "member" in {
      val veb = vanEmdeBoas(8)
      val seed = 1
      val rnd = new Random(seed)

      val original = (1 to 1000).map(_ => rnd.nextInt(veb.maxNumber+1))
      val lst = rnd.shuffle(original)
      lst foreach veb.insert

      (original map veb.member) should not contain (false)
      (0 to 100).toList.diff(original).map(veb.member) should not contain (true)
    }


  }
}
