package com.nachinius.vanEmdeBoas

import org.scalameter.api._

import scala.collection.immutable
import scala.util.Random

object vanEmdeBoas1 extends Bench.OfflineReport {

  performance of "van Emde Boas" in {
    val sizes = Gen.exponential("data size")(10,100000,2)
    val bits = 16
    val max = (1 << 7)-1
    val seed: Long = 34234325
    val searchN = 1000 // how many searches
    val rnd = new Random(seed)
    val data: Gen[immutable.IndexedSeq[Int]] = for {
      size <- sizes
    } yield (0 until size).map(_ => rnd.nextInt(max))
    val genvEB: Gen[vanEmdeBoas] = for {
      dt <- data
    } yield dt.foldLeft(vanEmdeBoas(bits)) {
      case (boas: vanEmdeBoas, i: Int) =>
        boas.insert(i)
        boas
    }
    val genvEBAndListToSearch: Gen[(vanEmdeBoas, immutable.IndexedSeq[Int])] = for {
      dt <- genvEB
      elem = (1 to searchN).map(_ => rnd.nextInt(max))
    } yield (dt,elem)

    measure method "insert vs n" in {
      using(data) in {
        lst =>
          val a = vanEmdeBoas(bits)
          lst.foreach {
            x => a.insert(x)
          }
      }
    }
    measure method "successor vs n" in {
      using(genvEBAndListToSearch) in {
        case (vEB, lst) => lst.foreach(vEB.successor)
      }
    }

  }

  performance of "successor vs bits" in {
    measure method "successor" in {
      val seed: Long = 11116793
      val n: Int = 50000
      val searches: Int = 1
      val rnd = new Random(seed)
      val sizes = Gen.range("bits")(4,30,1)
      val genBoas: Gen[vanEmdeBoas] = for {
        size <- sizes
        veb = vanEmdeBoas(size)
        lst = (1 to n).map(_ => rnd.nextInt(-1 + (1<<size)))
      } yield lst.foldLeft(veb) {
        case (boas: vanEmdeBoas, i: Int) => boas.insert(i);boas
      }
      val genBoasWithSearchData: Gen[(vanEmdeBoas, immutable.IndexedSeq[Int])] = for {
        boas <- genBoas
        elem = (1 to searches).map(_ => rnd.nextInt(boas.maxNumber))
      } yield (boas, elem)
      using(genBoasWithSearchData) in {
        case (veb, lst) => lst.foreach(veb.successor)
      }
    }
  }
}


