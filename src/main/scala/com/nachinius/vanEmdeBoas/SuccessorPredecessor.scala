package com.nachinius.vanEmdeBoas

/** Adds support for successor and predecessor queries **/
trait SuccessorPredecessor[T] {

  /** Return next element (not including itself) on this structure **/
  def successor(x: T): Option[T]

  /** Return previous element (not including itself) on this structure **/
  def predecessor(x: T): Option[T]
}



