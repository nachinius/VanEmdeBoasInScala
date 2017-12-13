package com.nachinius.vanEmdeBoas

import scala.collection.mutable
import scala.language.higherKinds

trait SuccessorPredecessor {

  type T
  type Set
  def successor(x: T): Option[T]
  def predecessor(x: T): Option[T] = successor(x) // @TODO
}

trait Membership[T] {
  def insert(x: T): Membership[T]
  def member(x: T): Boolean
}

object vanEmdeBoas {
  def apply[T](bits: Int): vanEmdeBoas = {
    if(bits == 1) new vEBSmall() else new vEB(bits)
  }
}

case class Upper(value: Int) extends AnyVal
case class Lower(value: Int) extends AnyVal

abstract class vanEmdeBoas extends Membership[Int] with Traversable[Int] with SuccessorPredecessor {
  type T = Int
  var min: Option[Int] = None
  var max: Option[Int] = None
  type Set = vanEmdeBoas
  val maxNumber: Int
}

class vEB(bits: Int) extends vanEmdeBoas {
  val maxNumber = (1 << bits)-1
  val minNumber = 0
  val halfbits: Int = bits / 2
  val lowerbits: Int = bits - bits / 2
  require(halfbits + lowerbits == bits)
  require(bits>1)

  override def foreach[U](f: T => U): Unit = {
    min.fold() { min =>
      f(min)
      cluster.foreach {
        case (z @ Upper(v) , boas: Set) =>
          boas.foreach(f)
      }
    }
  }

  val summary: vanEmdeBoas = vanEmdeBoas(halfbits)
  val cluster: mutable.Map[Upper,vanEmdeBoas] = mutable.Map()

  def getUpper: Int => Upper = x => Upper(x >>> halfbits)
  def getLower: Int => Lower = x => Lower(x & ((1 << halfbits) - 1))
  def expr: Int => (Upper,Lower) = x => (getUpper(x),getLower(x))
  def toNumber(c: Upper, l: Lower): Int = (c.value << halfbits) | l.value

  override def insert(x: T): Set = {
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
        cluster += (c -> vanEmdeBoas(bits/2))
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
          cluster.get(c).fold(false)((b: vanEmdeBoas) => b.member(l.value))
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
    max: Int <- cl.max if(lower< max)
    next <- cl.successor(lower)
  } yield toNumber(Upper(upper),Lower(next))

  def getSuccessorFromSummary(upper: Int, lower: Int): Option[Int] = for {
    successorClusterNumber <- summary.successor(upper)
    clusterOfSuccessor <- cluster.get(Upper(successorClusterNumber))
    successorLower <- clusterOfSuccessor.min
  } yield toNumber(Upper(successorClusterNumber),Lower(successorLower))


}


class vEBSmall() extends vanEmdeBoas {

  override val maxNumber: Int = 1

  override def successor(x: Int): Option[Int] =
    if(x == 0 && max.contains(1)) max else None

  override def foreach[U](f: T => U): Unit = {
    if(min.isDefined) {
      f(min)
      if(max.isDefined && max.get != min.get) {
        f(max)
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



