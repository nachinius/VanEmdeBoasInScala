package com.nachinius.vanEmdeBoas

// @mutable
trait vanEmdeBoas extends Membership[Int] with Traversable[Int] with SuccessorPredecessor[Int] {
  type T = Int
//  var min: Option[Int] = None
//  var max: Option[Int] = None
  val maxNumber: Int
  val bits: Int
  val halfbits: Int
  val lowerbits: Int

  def getUpper: Int => Upper = x => Upper(x >>> lowerbits)
  def getLower: Int => Lower = x => Lower(x & ((1 << lowerbits) - 1))
  def expr: Int => (Upper,Lower) = x => (getUpper(x),getLower(x))
  def toNumber(c: Upper, l: Lower): Int = (c.value << lowerbits) | l.value

  override def insert(x: T): vanEmdeBoas
}

case class Upper(value: Int) extends AnyVal
case class Lower(value: Int) extends AnyVal
