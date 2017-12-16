package com.nachinius.vanEmdeBoas.versionA

import com.nachinius.vanEmdeBoas.{Lower, Upper, vanEmdeBoas}

import scala.collection.mutable

object vanEmdeBoasA {
  /**
    * @param bits Bits used to store the numbers (w = log u). Numbers allowed will be in range
    *             [0,1,...,2^bits-1]
    */
  def apply[T](bits: Int): vanEmdeBoasA = {
    if(bits == 1) new vEBSmallA() else new vEBA(bits)
  }
}


// @mutable
abstract class vanEmdeBoasA extends vanEmdeBoas

class vEBA(override val bits: Int) extends vanEmdeBoasA {

  val maxNumber = (1 << bits)-1
  val minNumber = 0
  val halfbits: Int = math.ceil(0.5 * bits.toDouble).intValue()
  val lowerbits: Int = bits - halfbits
  require(halfbits + lowerbits == bits)
  require(bits>1)
  require(lowerbits > 0)
  require(maxNumber > 0) // avoid overflow of the number

  // don't use space for empty summaries
  lazy val summary: vanEmdeBoas = vanEmdeBoasA(halfbits)
  // using a hash table for cluster, we only store non empty ones
  val cluster: mutable.Map[Upper,vanEmdeBoasA] = mutable.Map()

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


  override def insert(x: T): Set = {
    if(x>maxNumber) {
      val message = s"$x is out of bounds (max=$maxNumber) from bits=$bits"
      throw new IllegalArgumentException("requirement failed: "+ message)
    }
    if(min.isEmpty) { // vEb is empty, very few to do and no recursions
      min = Some(x)
      max = Some(x)
    } else {

      val minimum = min.get
      var y: T = x
      if(y < minimum) {
        // swap x <-> minm
        y = minimum
        min = Some(x)
      }

      if(y > max.get) {
        max = Some(y)
      }

      val (c,i) = expr(y)
      if(cluster.get(c).isEmpty) {
        cluster += (c -> vanEmdeBoasA(lowerbits))
        summary.insert(c.value)
      }
      cluster.get(c).foreach( _.insert(i.value) )
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
        cluster.get(c).fold(false)((b: vanEmdeBoasA) => b.member(l.value))
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


class vEBSmallA() extends vanEmdeBoasA {

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




