package com.nachinius.vanEmdeBoas

import scala.collection.mutable
import scala.collection.mutable.Map

trait SuccessorPredecessor {

  type T
  type Set
  def successor(x: T): Option[T]
  def predecessor(x: T): Option[T]
}

trait Membership {
  type T
  type Set
  def insert(x: T): Set
  def member(x: T): Boolean
}

object vanEmdeBoas {
  def apply[T](bits: Int): vanEmdeBoas = {
    println(s"creating vEB of $bits bits")
    if(bits == 1) new vEBSmall() else new vEB(bits)
  }
}

case class Upper(value: Int) extends AnyVal
case class Lower(value: Int) extends AnyVal

abstract class vanEmdeBoas extends Membership {
  type T = Int
  var min: Option[Int] = None
  var max: Option[Int] = None
  type Set = vanEmdeBoas
}

class vEB(bits: Int) extends vanEmdeBoas {
  val halfbits: Int = bits / 2
  val lowerbits: Int = bits - bits / 2
  require(halfbits + lowerbits == bits)

  val summary: vanEmdeBoas = vanEmdeBoas(halfbits)
  val cluster: mutable.Map[Upper,vanEmdeBoas] = mutable.Map()

  def getUpper: Int => Upper = x => Upper(x >>> halfbits)
  def getLower: Int => Lower = x => Lower(x & (1 << halfbits - 1))
  def expr: Int => (Upper,Lower) = x => (getUpper(x),getLower(x))

  override def insert(x: T): Set = {
    println(s"inserting $x")
    if(min.isEmpty) { // vEb is empty, very few to do and no recursions
      min = Some(x)
      max = Some(x)
    } else {

      var minimum = min.get
      var y: T = x
      if(y < minimum) {
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

  override def member(x: Int): Boolean = {
    min match {
      case None => false // no min, so is completely empty
      case Some(mn) => if(x == mn || max.contains(x)) true else {
        if(max.get > x) false else {
          val (c, l) = expr(x)
          cluster.get(c).fold(false)((b: vanEmdeBoas) => b.member(l.value))
        }
      }
    }
  }
}


class vEBSmall extends vanEmdeBoas {
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



