package com.nachinius.vanEmdeBoas.versionA

import com.nachinius.vanEmdeBoas.{Lower, Upper, vanEmdeBoas}

import scala.collection.mutable

object VersionA {
  /**
    * @param bits Bits used to store the numbers (w = log u). Numbers allowed will be in range
    *             [0,1,...,2^bits-1]
    */
  def apply[T](bits: Int): VersionA = {
    if(bits == 1) new SmallVEBVersionA() else new BigVEBVersionA(bits)
  }
}


// @mutable
abstract class VersionA extends vanEmdeBoas

class BigVEBVersionA(override val bits: Int) extends VersionA {

  val maxNumber = (1 << bits)-1
  val minNumber = 0
  val halfbits: Int = math.ceil(0.5 * bits.toDouble).intValue()
  val lowerbits: Int = bits - halfbits
  require(halfbits + lowerbits == bits)
  require(bits>1)
  require(lowerbits > 0)
  require(maxNumber > 0) // avoid overflow of the number

  // don't use space for empty summaries
  lazy val summary: vanEmdeBoas = VersionA(halfbits)
  // using a hash table for cluster, we only store non empty ones
  val cluster: mutable.Map[Upper,VersionA] = mutable.Map()

  override def toString(): String = {
    s"vEB($bits($halfbits++$lowerbits)=>[$minNumber,$maxNumber],,,clusters=${cluster.size}"
  }

  override def foreach[U](f: T => U): Unit = {
    min.fold({}) { min =>
      f(min)
      cluster.foreach {
        case (z @ Upper(v) , boas: Set) =>
          boas.foreach(x => f(v * (1<<lowerbits) + x))
      }
    }
  }

  def insertIntoMinAndMax(t: T): Unit = {
    min = Some(t)
    max = Some(t)
  }

  override def insert(x: T): Set = {
    if(x>maxNumber) {
      val message = s"$x is out of bounds (max=$maxNumber) from bits=$bits"
      throw new IllegalArgumentException("requirement failed: "+ message)
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
        val (nextMinValue, pushValue) = if(x<minValue) (x,minValue) else (minValue, x)
        min = Some(nextMinValue)

        // new max
        max = max.map(prev => if(prev<pushValue) pushValue else prev)

        val (c,i) = expr(pushValue)
        // create cluster if necessary and add to summary
        if (cluster.get(c).isEmpty) {
          cluster += (c -> VersionA(lowerbits))
          summary.insert(c.value)
        }
        // recurse for insertion
        cluster.get(c).foreach(_.insert(i.value))
    }
    this
  }

  override def member(x: Int): Boolean = min match {
    case None => false // no min, so is completely empty
    case Some(mn) =>
      if(x == mn || max.contains(x)) true
      else if(max.nonEmpty && max.get < x) false
      else {
        val (c, l) = expr(x)
        cluster.get(c).fold(false)((b: VersionA) => b.member(l.value))
      }
  }


  // SuccessorPredecessor
  override def successor(x: Int): Option[Int] = min match {
    case None => None // no min, empty veb, impossible successor
    case Some(m) if x<m => min // trivial successor
    case _ => // need to look inside
      val (c, l) = expr(x)
      getSuccessorInsideCluster(c.value,l.value) orElse getSuccessorFromSummary(c.value,l.value)
  }

  def getSuccessorInsideCluster(upper: Int, lower: Int): Option[Int] = for {
    cl <- cluster.get(Upper(upper))
    max: Int <- cl.max if lower < max
    next <- cl.successor(lower)
  } yield toNumber(Upper(upper),Lower(next))

  def getSuccessorFromSummary(upper: Int, lower: Int): Option[Int] = for {
    successorClusterNumber <- summary.successor(upper)
    clusterOfSuccessor <- cluster.get(Upper(successorClusterNumber))
    successorLower <- clusterOfSuccessor.min
  } yield toNumber(Upper(successorClusterNumber),Lower(successorLower))


}


class SmallVEBVersionA() extends VersionA {

  override val bits: Int = 1
  override val halfbits: Int = 0
  override val lowerbits: Int = 1
  override val maxNumber: Int = 1

  override def successor(x: Int): Option[Int] =
    if(x == 0 && max.contains(1)) max else None

  override def foreach[U](f: T => U): Unit = {
    if(min.isDefined) {
      f(min.get)
      if(max.isDefined && max.get != min.get) {
        f(max.get)
      }
    }
  }

  override def member(x: Int): Boolean = {
    if(x==0) min.contains(0)
    else if(x==1) max.contains(1)
    else false
  }

  override def insert(x: Int): Set = {
    if(x==0) {
      min = Some(0)
      if(max.isEmpty) max = Some(0)
    }
    else if(x==1) {
      max = Some(1)
      if(min.isEmpty) min = Some(1)
    }
    this
  }
}




