package com.nachinius.vanEmdeBoas

import com.nachinius.vanEmdeBoas.immutable.ImmutableVanEmdeBoas
import com.nachinius.vanEmdeBoas.mutable.mutableVanEmdeBoas
import org.scalameter.{Bench, Gen, Measurer}

import scala.util.Random

object ComparisionMemoryVanEmdeBoasMutableAndImmutableTestPerformance extends Bench.LocalTime {
  override def measurer  = new Measurer.MemoryFootprint() {}

  val seed: Long = 11116793
  val n: Int = 500
  val searches: Int = 1000

  performance of "successor vs bits (immutable)" in {
    measure method "successor" in {
      val rnd = new Random(seed)
      val bits = Gen.range("bits")(9,30,1)
      val genBoas: Gen[vanEmdeBoas] = for {
        bit <- bits
        veb = ImmutableVanEmdeBoas(bit).asInstanceOf[vanEmdeBoas]
        lst = (1 to n).map(_ => rnd.nextInt(veb.maxNumber)).distinct
      } yield lst.foldLeft(veb) {
        case (boas, i) => boas.insert(i)
      }
      val genBoasWithSearchData = for {
        boas <- genBoas
        elem = (1 to searches).map(_ => rnd.nextInt(boas.maxNumber))
      } yield (boas, elem)
      using(genBoasWithSearchData) in {
        case (veb, lst) => //lst.map(veb.successor)
          veb
      }
    }
  }

  performance of "successor vs bits (mutable)" in {
    measure method "successor" in {
      val rnd = new Random(seed)
      val bits = Gen.range("bits")(9,30,1)
      val genBoas: Gen[mutableVanEmdeBoas] = for {
        bit <- bits
        veb = mutableVanEmdeBoas(bit)
        lst = (1 to n).map(_ => rnd.nextInt(veb.maxNumber)).distinct
      } yield lst.foldLeft(veb) {
        case (boas, i) => boas.insert(i);boas
      }
      val genBoasWithSearchData = for {
        boas <- genBoas
        elem = (1 to searches).map(_ => rnd.nextInt(boas.maxNumber))
      } yield (boas, elem)
      using(genBoasWithSearchData) in {
        case (veb, lst) => //lst.map(veb.successor)
          veb
      }
    }
  }

}

