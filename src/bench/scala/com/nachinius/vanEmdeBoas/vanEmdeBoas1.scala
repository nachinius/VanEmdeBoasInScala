package com.nachinius.vanEmdeBoas

import org.scalameter.api._

import scala.collection.immutable
import scala.util.Random

object vanEmdeBoas1 extends Bench.OfflineReport {
  val sizes = Gen.exponential("data size")(10,100000,2)
  val max = (1 << 7)-1
  val seed: Long = 34234325
  val rnd = new Random(seed)
  val data: Gen[immutable.IndexedSeq[Int]] = for {
    size <- sizes
  } yield (0 until size).map(_ => rnd.nextInt(max))
  val genvEB: Gen[vanEmdeBoas] = for {
    dt <- data
  } yield dt.foldLeft(vanEmdeBoas(16)) {
    case (boas: vanEmdeBoas, i: Int) =>
      boas.insert(i)
      boas
  }
  val genvEBAndListToSearch: Gen[(vanEmdeBoas, immutable.IndexedSeq[Int])] = for {
    dt <- genvEB
    elem = (1 to 1000).map(_ => rnd.nextInt(max))
  } yield (dt,elem)

  performance of "van Emde Boas" in {
    measure method "insert vs n" in {
      using(data) in {
        lst =>
          val a = vanEmdeBoas(16)
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

    measure method "successor vs bits" in {
      val sizes = Gen.range("bits")(4,20,1)
      val genBoas: Gen[vanEmdeBoas] = for {
        size <- sizes
        veb = vanEmdeBoas(size)
        lst = (1 to 1 << 5).map(_ => rnd.nextInt(-1 + (1<<size)))
      } yield lst.foldLeft(veb) {
        case (boas: vanEmdeBoas, i: Int) => boas.insert(i);boas
      }
      val genBoasWithSearchData: Gen[(vanEmdeBoas, immutable.IndexedSeq[Int])] = for {
        boas <- genBoas
        elem = (1 to 1000).map(_ => rnd.nextInt(boas.maxNumber))
      } yield (boas, elem)
      using(genBoasWithSearchData) in {
        case (veb, lst) => lst.foreach(veb.successor)
      }
    }
  }
}
