package com.nachinius.vanEmdeBoas

import org.scalameter.api
import org.scalameter.api._

import scala.collection.immutable
import scala.util.Random

object vanEmdeBoasAll extends Bench.Group {
  include(new vanEmdeBoasAMemory { })
  include(new vanEmdeBoasAPerformance { })
}

trait vanEmdeBoasAMemory extends Bench.OfflineReport {

  override def measurer: Measurer[Double] = new api.Measurer.BaseMemoryFootprint {}

  val sizes = Gen.exponential("data size")(10,100000,2)
  val bits = Gen.range("w bits")(10,30,2)
  val gen = for {
    size <- sizes
    bit <- bits
  } yield (bit, size)
  val seed = 59797345
  val rnd = new Random(seed)
  performance of "vEB" in {
    measure method "memory footprint vs n" in {
      using(gen) in {
        case (bit, size) =>
          val veb = vanEmdeBoasA(bit)
          (1 to size).map(_ => rnd.nextInt(veb.maxNumber)).foreach(veb.insert)
          veb
      }
    }
  }
}
trait vanEmdeBoasAPerformance extends Bench.OfflineReport {

  performance of "van Emde Boas" in {
    val sizes = Gen.exponential("data size")(8,100000,2)
    val bits = 16
    val maxInt = (1 << 7)-1
    val seed: Long = 34234325
    val searchN = 1 // how many searches
    val rnd = new Random(seed)
    val data: Gen[immutable.IndexedSeq[Int]] = for {
      size <- sizes
    } yield (0 until size).map(_ => rnd.nextInt(maxInt))
    val genvEB: Gen[vanEmdeBoasA] = for {
      dt <- data
    } yield dt.foldLeft(vanEmdeBoasA(bits)) {
      case (boas: vanEmdeBoasA, i: Int) =>
        boas.insert(i)
        boas
    }
    val genvEBAndListToSearch: Gen[(vanEmdeBoasA, immutable.IndexedSeq[Int])] = for {
      dt <- genvEB
      elem = (1 to searchN).map(_ => rnd.nextInt(maxInt))
    } yield (dt,elem)

    measure method "insert vs n" in {
      using(data) in {
        lst =>
          val a = vanEmdeBoasA(bits)
          lst.foreach {
            x => a.insert(x)
          }
      }
    }
    measure method "successor vs n" in {
      using(genvEBAndListToSearch) in {
        case (vEB, lst) => lst.map(vEB.successor)
      }
    }

  }

  performance of "successor vs bits" in {
    measure method "successor" in {
      val seed: Long = 11116793
      val n: Int = 50000
      val searches: Int = 1
      val rnd = new Random(seed)
      val bits = Gen.range("bits")(4,30,1)
      val genBoas: Gen[vanEmdeBoasA] = for {
        bit <- bits
        veb = vanEmdeBoasA(bit)
        lst = (1 to n).map(_ => rnd.nextInt(veb.maxNumber)).distinct
      } yield lst.foldLeft(veb) {
        case (boas: vanEmdeBoasA, i: Int) => boas.insert(i);boas
      }
      val genBoasWithSearchData: Gen[(vanEmdeBoasA, immutable.IndexedSeq[Int])] = for {
        boas <- genBoas
        elem = (1 to searches).map(_ => rnd.nextInt(boas.maxNumber))
      } yield (boas, elem)
      using(genBoasWithSearchData) in {
        case (veb, lst) => lst.map(veb.successor)
      }
    }
  }
}




