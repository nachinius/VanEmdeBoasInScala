package com.nachinius.vanEmdeBoas.versionB

import com.nachinius.vanEmdeBoas.vanEmdeBoas
import com.nachinius.vanEmdeBoas.versionA.VersionA
import org.scalameter.{Bench, Gen}

import scala.collection.immutable
import scala.util.Random

object immutableVanEmdeBoasPerformance extends Bench.OfflineReport {

  val seed: Long = 11116793
  val n: Int = 50000
  val searches: Int = 1000

  performance of "successor vs bits (immutable)" in {
    measure method "successor" in {
      val rnd = new Random(seed)
      val bits = Gen.range("bits")(4,30,1)
      val genBoas: Gen[vanEmdeBoas] = for {
        bit <- bits
        veb = versionB(bit).asInstanceOf[vanEmdeBoas]
        lst = (1 to n).map(_ => rnd.nextInt(veb.maxNumber)).distinct
      } yield lst.foldLeft(veb) {
        case (boas, i) => boas.insert(i)
      }
      val genBoasWithSearchData = for {
        boas <- genBoas
        elem = (1 to searches).map(_ => rnd.nextInt(boas.maxNumber))
      } yield (boas, elem)
      using(genBoasWithSearchData) in {
        case (veb, lst) => lst.map(veb.successor)
      }
    }
  }

  performance of "successor vs bits (mutable)" in {
    measure method "successor" in {
      val rnd = new Random(seed)
      val bits = Gen.range("bits")(4,30,1)
      val genBoas: Gen[VersionA] = for {
        bit <- bits
        veb = VersionA(bit)
        lst = (1 to n).map(_ => rnd.nextInt(veb.maxNumber)).distinct
      } yield lst.foldLeft(veb) {
        case (boas, i) => boas.insert(i);boas
      }
      val genBoasWithSearchData: Gen[(VersionA, immutable.IndexedSeq[Int])] = for {
        boas <- genBoas
        elem = (1 to searches).map(_ => rnd.nextInt(boas.maxNumber))
      } yield (boas, elem)
      using(genBoasWithSearchData) in {
        case (veb, lst) => lst.map(veb.successor)
      }
    }
  }
}

