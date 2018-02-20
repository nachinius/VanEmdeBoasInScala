package com.nachinius.vanEmdeBoas

import org.scalatest.{FreeSpec, Matchers}

import scala.collection.{immutable => im, mutable => mut}
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
    "insert" in {
      val a = constructorVanEmdeBoas(8)
      assert(a.insert(3).member(3))
      assert(a.insert(4).member(4))
      assert(a.insert(3).insert(4).member(3))
      val i = (1 << 7) - 1
      assert(a.insert(i).member( i))
      assert(a.insert(0).member(0))
    }
    "member" - {
      "should report correctly" in {
        val veb = constructorVanEmdeBoas(8)
        val seed = 1
        val rnd = new Random(seed)
        val n = 1000

        val original: im.Seq[Int] = (1 to n).map(_ => rnd.nextInt(veb.maxNumber)).distinct
        val lst = rnd.shuffle(original)
        val testable: vanEmdeBoas = lst.foldLeft(veb)((boas: vanEmdeBoas, i: Int) => boas.insert(i))

        (original map testable.member) shouldBe (original map { _ => true })
        (0 to 100).toList.diff(original).map(testable.member) should not contain (true)
      }
    }
    "foreach" - {
      "must walk all inserted numbers" in {
        val seed: Long = 12758345
        val numbersToInsert = 300
        val rnd = new Random(seed)
        val veb = constructorVanEmdeBoas(16)
        val numbers = ((0 to 255) ++ (1 to numbersToInsert).map(_ => rnd.nextInt(veb.maxNumber))).distinct
        // insert
        val testable = numbers.foldLeft(veb)((boas: vanEmdeBoas, i: Int) => boas.insert(i))
        // collect
        val result = mut.ArrayBuffer[Int]()
        testable.foreach(x => result += x)
        // compare
        result.sorted should be (result.distinct.sorted)
        val diff = numbers.toSet.diff(result.toSet)
        diff shouldBe empty
        val diff2 = result.toSet.diff(numbers.toSet)
        diff2 shouldBe empty
      }

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
          val testable = lst.foldLeft(veb) {case (boas: vanEmdeBoas, i: Int) => boas.insert(i)}

          testable.successor(2) shouldBe Some(5)
          testable.successor(5) shouldBe Some(8)
          testable.successor(6) shouldBe Some(8)
          testable.successor(66) shouldBe None
          testable.successor(0) shouldBe Some(2)
          testable.member(24) shouldBe true
          testable.successor(21) shouldBe Some(24)

          val results = original map testable.successor
          val expectedSuccessor = original.tail.map(Some(_)) :+ None


          results shouldBe expectedSuccessor

          (original map testable.member) shouldBe original.map(_ => true)
        }

        "when bits are odd" in {
          constructorVanEmdeBoas(10).insert(20).insert(24).insert(10).successor(21) shouldBe Some(24)
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

    "predecessor" - {
      "should shield the following integer in set" - {
        def successorTest(bits: Int): Any = {
          val veb = constructorVanEmdeBoas(bits)
          val seed: Long = 123423
          val rnd = new Random(seed)
          val original = List(2, 5, 8, 12, 16, 20, 24, 30, 31, 32, 33, 50, 54)
          val lst = rnd.shuffle(original)
          val testable = lst.foldLeft(veb) {case (boas: vanEmdeBoas, i: Int) => boas.insert(i)}

          testable.predecessor(2) shouldBe None
          testable.predecessor(5) shouldBe Some(2)
          testable.predecessor(6) shouldBe Some(5)
          testable.predecessor(66) shouldBe Some(54)
          testable.predecessor(0) shouldBe None
          testable.member(24) shouldBe true
          testable.predecessor(21) shouldBe Some(20)

          val results = original map testable.predecessor
          val expectedPredecessor = None +: original.init.map(Some(_))


          results shouldBe expectedPredecessor

          (original map testable.member) shouldBe original.map(_ => true)
        }

        "when bits are odd" in {
          constructorVanEmdeBoas(10).insert(20).insert(24).insert(10).predecessor(21) shouldBe Some(20)
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

    "delete" - {
      "the only element" in {
        val veb = constructorVanEmdeBoas(16)
        veb.insert(5).member(5) shouldBe true
        veb.delete(5).member(5) shouldBe false
      }
      "the max element" in {
        val veb = constructorVanEmdeBoas(16)
        veb.insert(3).insert(12).member(12) shouldBe true
        veb.delete(12).member(12) shouldBe false
      }
      "simple example" in {
        val veb = constructorVanEmdeBoas(16)
        veb.insert(5).member(5) shouldBe true
        veb.delete(5).member(5) shouldBe false
        veb.insert(3).insert(8).insert(9).insert(12)
        veb.delete(12).member(12) shouldBe false
        veb.delete(8).member(8) shouldBe false
        veb.insert(8).delete(3).member(3) shouldBe false
        veb.member(8) shouldBe true
        veb.insert(3).member(8) shouldBe true
        veb.member(3) shouldBe true
        veb.delete(12).member(12) shouldBe false
      }
      "in large structures" - {
        val seed: Long = 435784L
        val rnd = new Random(seed)
        val n = 1 << 7
        val bits = 8
        val initialVEB = constructorVanEmdeBoas(bits)
        val maxValue = initialVEB.maxNumber
        val numbers = (0 until n).map(_ => rnd.nextInt(maxValue)).distinct
        val veb = numbers.tail.foldLeft(initialVEB)((boas, i) => boas.insert(i))
        val notInserted = numbers.head
        veb.member(notInserted) shouldBe false
        "an unexisting number should return same structure" in {
          val previous = veb.toSeq
          val next = veb.delete(notInserted)
          next.member(notInserted) shouldBe false
          next.toSeq should contain theSameElementsAs previous
        }
        "an existing number" - {
          "should take it out" in {
            // chose one
            val number = numbers(rnd.nextInt(numbers.tail.length))
            val remaining = numbers.diff(number :: Nil)
            val allExceptOne = veb.delete(number)
            allExceptOne.member(number) shouldBe false
            allExceptOne.toSet -- remaining shouldBe Set()
          }
        }
      }
      "small veb" in {
        val veb = constructorVanEmdeBoas(1)
        veb.insert(0).insert(1)
        veb.toSeq should contain theSameElementsAs Seq(0,1)
        veb.delete(0).delete(1)
        veb.toSeq should have length 0
      }
      "correctly for smalls veb" in {
        val bits = 2
        val n = 1 << bits
        val veb: vanEmdeBoas = constructorVanEmdeBoas(bits)
        val list = (0 until n).toVector
        val boas = list.foldLeft(veb) { case (boas: vanEmdeBoas, i: Int) =>
            boas.insert(i)
        }
        val listToRemove = list
        val endBoas = listToRemove.foldLeft(boas) {
          case (boas: vanEmdeBoas, i: Int) => boas.delete(i)
        }
        endBoas.toSeq should have length 0
      }

    }



  }

}
