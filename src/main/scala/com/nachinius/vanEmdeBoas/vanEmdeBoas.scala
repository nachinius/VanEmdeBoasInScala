package com.nachinius.vanEmdeBoas

/**
  * This strucuture handles O(lg w) queries for successor and predecessor, where w is the bits
  * used to represent the ordering.
  */
trait vanEmdeBoas extends
  Membership[Int] with
  SuccessorPredecessor[Int] with
  Traversable[Int] {

  type T = Int
  val maxNumber: Int
  val bits: Int
  val halfbits: Int
  val lowerbits: Int

//  def min
//
//  def max

  def expr: Int => (Upper, Lower) = x => (getUpper(x), getLower(x))

  def getUpper: Int => Upper = x => Upper(x >>> lowerbits)

  def getLower: Int => Lower = x => Lower(x & ((1 << lowerbits) - 1))

  def toNumber(c: Upper, l: Lower): Int = (c.value << lowerbits) | l.value

  override def insert(x: T): vanEmdeBoas
}

case class Upper(value: Int) extends AnyVal

case class Lower(value: Int) extends AnyVal
