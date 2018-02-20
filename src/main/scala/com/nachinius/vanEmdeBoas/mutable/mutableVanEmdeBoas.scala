package com.nachinius.vanEmdeBoas.mutable

import com.nachinius.vanEmdeBoas.{Lower, Upper, vanEmdeBoas}

import scala.collection.mutable

object mutableVanEmdeBoas {
  /**
    * @param bits Bits used to store the numbers (w = log u). Numbers allowed will be in range
    *             [0,1,...,2^bits-1]
    **/
  def apply[T](bits: Int): mutableVanEmdeBoas = {
    if (bits == 1) new SmallVEBMutableVanEmdeBoas() else new BigVEBMutableVanEmdeBoas(bits)
  }
}


// @mutable
abstract class mutableVanEmdeBoas extends vanEmdeBoas {
  var min: Option[Int] = None
  var max: Option[Int] = None

  def delete(x: T): mutableVanEmdeBoas
}

class BigVEBMutableVanEmdeBoas(override val bits: Int) extends mutableVanEmdeBoas {

  // don't use space for empty summaries
  val maxNumber = (1 << bits) - 1
  val minNumber = 0
  val halfbits: Int = math.ceil(0.5 * bits.toDouble).intValue()
  val lowerbits: Int = bits - halfbits
  // using a hash table for cluster, we only store non empty ones
  val cluster: mutable.Map[Upper, mutableVanEmdeBoas] = mutable.Map()
  var optSummary: Option[mutableVanEmdeBoas] = None

  override def toString(): String = {
    s"vEB($bits($halfbits++$lowerbits)=>[$minNumber,$maxNumber],,,clusters=${cluster.size}"
  }

  require(halfbits + lowerbits == bits)
  require(bits > 1)
  require(lowerbits > 0)
  require(maxNumber > 0) // avoid overflow of the number

  override def foreach[U](f: T => U): Unit = {
    min.fold({}) { min =>
      f(min)
      cluster.foreach {
        case (z@Upper(v), boas) =>
          boas.foreach(x => f(v * (1 << lowerbits) + x))
      }
    }
  }

  override def insert(x: T): vanEmdeBoas = {
    if (x > maxNumber) {
      val message = s"$x is out of bounds (max=$maxNumber) from bits=$bits"
      throw new IllegalArgumentException("requirement failed: " + message)
    }
    min.filterNot(
      // don't do doble insert
      minValue => x == minValue
    ).fold(
      // no minimum, first set of min (and max)
      insertIntoMinAndMax(x)
    ) {
      minValue =>
        // swap x & minValue if necessary
        val (nextMinValue, pushValue) = if (x < minValue) (x, minValue) else (minValue, x)
        min = Some(nextMinValue)

        // new max
        max = max.map(prev => if (prev < pushValue) pushValue else prev)

        val (c, i) = expr(pushValue)
        // create cluster if necessary and add to summary
        if (cluster.get(c).isEmpty) {
          cluster += (c -> mutableVanEmdeBoas(lowerbits))
          getSummaryAndCreateIfNecessary.insert(c.value)
        }
        // recurse for insertion
        cluster.get(c).foreach(_.insert(i.value))
    }
    this
  }

  def getSummaryAndCreateIfNecessary: mutableVanEmdeBoas = {
    optSummary.getOrElse(createSummary)
  }

  def createSummary: mutableVanEmdeBoas = {
    optSummary = Some(mutableVanEmdeBoas(halfbits))
    optSummary.get
  }

  def insertIntoMinAndMax(t: T): Unit = {
    min = Some(t)
    max = Some(t)
  }

  override def member(x: Int): Boolean = min match {
    case None => false // no min, so is completely empty
    case Some(mn) =>
      if (x == mn || max.contains(x)) true
      else if (max.nonEmpty && max.get < x) false
      else {
        val (c, l) = expr(x)
        cluster.get(c).fold(false)((b: mutableVanEmdeBoas) => b.member(l.value))
      }
  }


  // SuccessorPredecessor
  override def successor(x: Int): Option[Int] = min match {
    case None => None // no min, empty veb, impossible successor
    case Some(m) if x < m => min // trivial successor
    case _ => // need to look inside
      val (c, l) = expr(x)
      getSuccessorInsideCluster(c.value, l.value) orElse getSuccessorFromSummary(c.value, l.value)
  }

  def getSuccessorInsideCluster(upper: Int, lower: Int): Option[Int] = for {
    cl <- cluster.get(Upper(upper))
    max: Int <- cl.max if lower < max
    next <- cl.successor(lower)
  } yield toNumber(Upper(upper), Lower(next))

  def getSuccessorFromSummary(upper: Int, lower: Int): Option[Int] = for {
    summary <- optSummary
    successorClusterNumber <- summary.successor(upper)
    clusterOfSuccessor <- cluster.get(Upper(successorClusterNumber))
    successorLower <- clusterOfSuccessor.min
  } yield toNumber(Upper(successorClusterNumber), Lower(successorLower))

  override def predecessor(x: T): Option[T] = {
    max match {
      case None => None // no max, empty veb, no predecessor in here
      case Some(m) if x > m => max // trivial predecessor
      case _ => // x <= max.get
        min match {
          //        case None => None // is ruled out because there is a max
          case Some(mi) if x <= mi => None // not in here
          case _ => // it must be inside
            val (c, l) = expr(x)
            getPredecessorInsideCluster(c.value, l.value) orElse getPredecessorFromSummary(c.value, l.value) orElse min
        }
    }
  }

  def getPredecessorInsideCluster(upper: Int, lower: Int): Option[T] = for {
    cl <- cluster.get(Upper(upper))
    min: Int <- cl.min if lower > min
    next <- cl.predecessor(lower)
  } yield toNumber(Upper(upper), Lower(next))

  def getPredecessorFromSummary(upper: Int, lower: Int): Option[Int] = for {
    summary <- optSummary
    predecessorClusterNumber <- summary.predecessor(upper)
    clusterOfPredecessor <- cluster.get(Upper(predecessorClusterNumber))
    predecessorLower <- clusterOfPredecessor.max
  } yield toNumber(Upper(predecessorClusterNumber), Lower(predecessorLower))

  def delete(x: T): mutableVanEmdeBoas = {
    (min,max) match {
      case (Some(mn),Some(mx)) if mn <= x && x <= mx => {
        var (c, l) = expr(x)
        if (mn == x) {
          val nextMin = for { // calculate new min
            summary <- optSummary
            summMin <- summary.min // which should be in the cluster with the minimal number
            upperC = Upper(summMin)
            cl <- cluster.get(upperC) // getting the cluster
            lowerBitMin <- cl.min
          } yield (upperC, Lower(lowerBitMin))
          if (nextMin.isEmpty) {
            // we become empty
            min = None
            return this
          } else {
            // we have a new min, thus, we also need to delete from below structure the
            // new min
            nextMin foreach {
              x => {
                min = Some(toNumber(x._1,x._2))
                c = x._1
                l = x._2
              }
            }
          }
        } // end mn == x

        cluster.get(c).
          map(_.delete(l.value)). // the deleted value, is inside cluster `c`
          filter(_.min.isEmpty). // if such cluster become empty, clean it the `c` from the summary
          foreach(_ => {
              optSummary.map(sum => sum.delete(c.value)).foreach( _ => cluster.remove(c))
            }
          )

        // if summary doesn't have a min (is empty) and thus, new max is the min
        optSummary.filter(_.min.isEmpty).foreach { _ =>
          optSummary = None
          max = min
        }
        optSummary.filter(_.min.nonEmpty).foreach {
          summary =>
            max = for {
              mx <- summary.max
              cl <- cluster.get(Upper(mx))
              y <- cl.max
            } yield toNumber(Upper(mx), Lower(y))
        }
        this
      }
      case _ => this
    }
  }
}

class SmallVEBMutableVanEmdeBoas() extends mutableVanEmdeBoas {

  override val bits: Int = 1
  override val halfbits: Int = 0
  override val lowerbits: Int = 1
  override val maxNumber: Int = 1

  override def successor(x: Int): Option[Int] =
    if (x == 0 && max.contains(1)) max else None

  override def predecessor(x: T): Option[T] =
    if (x == 1 && min.contains(0)) min else None

  override def foreach[U](f: T => U): Unit = {
    if (min.isDefined) {
      f(min.get)
      if (max.isDefined && max.get != min.get) {
        f(max.get)
      }
    }
  }

  override def member(x: Int): Boolean = {
    if (x == 0) min.contains(0)
    else if (x == 1) max.contains(1)
    else false
  }

  override def insert(x: Int) = {
    if (x == 0) {
      min = Some(0)
      if (max.isEmpty) max = Some(0)
    }
    else if (x == 1) {
      max = Some(1)
      if (min.isEmpty) min = Some(1)
    }
    this
  }

  override def delete(x: T): mutableVanEmdeBoas = {
    if (x == 0) {
      if (max.contains(x)) {
        min = None
        max = None
      } else {
        min = max
      }
    } else { // x == 1
      if (min.contains(x)) {
        min = None
        max = None
      } else {
        max = None
      }
    }
    this
  }
}






