package com.nachinius.vanEmdeBoas

import org.scalatest.{FreeSpec, Matchers}

import scala.collection.{immutable, mutable}
import scala.util.Random

trait vanEmdeBoasTest extends FreeSpec with Matchers {
  def constructorVanEmdeBoas: Int => vanEmdeBoas

  "Methods tests" - {

    "construction of odd bits vEB should work" in {
      constructorVanEmdeBoas(5).halfbits shouldBe 3
      constructorVanEmdeBoas(5).lowerbits shouldBe 2
    }
    "maxNumber" in {
      val veb = constructorVanEmdeBoas(8)
      veb.maxNumber shouldBe 2*2*2*2*2*2*2*2-1
      constructorVanEmdeBoas(4).maxNumber shouldBe 15
      constructorVanEmdeBoas(16).maxNumber shouldBe (1<<16)-1
      constructorVanEmdeBoas(16).maxNumber should be > 0
      constructorVanEmdeBoas(12).maxNumber should be > 0
      constructorVanEmdeBoas(21).maxNumber should be > 0
      constructorVanEmdeBoas(5).maxNumber should be > 0
      constructorVanEmdeBoas(30).maxNumber should be > 0
    }
    "member" in {
      val veb = constructorVanEmdeBoas(8)
      val seed = 1
      val rnd = new Random(seed)

      val original: immutable.Seq[Int] = (1 to 1000).map(_ => rnd.nextInt(veb.maxNumber)).distinct
      val lst = rnd.shuffle(original)
      lst foreach veb.insert

      (original map veb.member) shouldBe (original map { _ => true})
      (0 to 100).toList.diff(original).map(veb.member) should not contain (true)
    }

    "insert" in {
      val a = constructorVanEmdeBoas(8)
      assert(a.insert(3).member(3))
      assert(a.insert(4).member(3))
      assert(a.member(4))
      val i = (1 << 7) - 1
      assert(a.insert(i).member( i))
    }

    "lowerbits && upperbits" - {
      "when bits are odd" - {
        "must shield correct value" in {
          val v = constructorVanEmdeBoas(5)
          v.expr(20) should be ((Upper(5),Lower(0)))
          v.expr(21) should be ((Upper(5),Lower(1)))
          v.expr(22) should be ((Upper(5),Lower(2)))
          v.expr(23) should be ((Upper(5),Lower(3)))
          v.expr(24) should be ((Upper(6),Lower(0)))
          v.expr(25) should be ((Upper(6),Lower(1)))
        }
      }
    }

    "successor" - {
      "should shield the following integer in set" - {
        def successorTest(bits: Int): Any = {
          val veb = constructorVanEmdeBoas(bits)
          val seed: Long = 123423
          val rnd = new Random(seed)
          val original = List(2, 5, 8, 12, 16, 20, 24, 30, 31, 32, 33, 50, 54)
          val lst = rnd.shuffle(original)
          // test
          lst foreach veb.insert

          veb.successor(2) shouldBe Some(5)
          veb.successor(5) shouldBe Some(8)
          veb.successor(6) shouldBe Some(8)
          veb.successor(66) shouldBe None
          veb.successor(0) shouldBe Some(2)
          veb.member(24) shouldBe true
          veb.successor(21) shouldBe Some(24)

          val results = original map veb.successor
          val expectedSuccessor = original.tail.map(Some(_)) :+ None


          results shouldBe expectedSuccessor

          (original map veb.member) shouldBe original.map(_ => true)
        }

        "when bits are odd" in {
          val veb = constructorVanEmdeBoas(10)
          veb.insert(20)
          veb.insert(24)
          veb.insert(10)
          veb.successor(21) shouldBe Some(24)
        }
        "for most bits sizes" in {
          (8 to 30 by 1).foreach {
            bits =>
              withClue(s"In bits = $bits") {
                successorTest(bits)
              }
          }
        }

      }
    }

    "foreach" - {
      "must walk all numbers" in {
        val seed: Long = 12758345
        val numbersToInsert = 1000
        val rnd = new Random(seed)
        val veb = constructorVanEmdeBoas(16)
        val numbers = ((0 to 255) ++ (1 to numbersToInsert).map(_ => rnd.nextInt(veb.maxNumber))).distinct
        // insert
        numbers.foreach(veb.insert)
        // collect
        val result = mutable.ArrayBuffer[Int]()
        veb.foreach(x => result += x)
        // compare
        result.sorted should be (result.distinct.sorted)
        val diff = numbers.toSet.diff(result.toSet)
        diff shouldBe empty
        val diff2 = result.toSet.diff(numbers.toSet)
        diff2 shouldBe empty
      }

    }



  }

}
