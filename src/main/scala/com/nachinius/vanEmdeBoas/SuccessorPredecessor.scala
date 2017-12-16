package com.nachinius.vanEmdeBoas

trait SuccessorPredecessor {

  type T
  type Set
  def successor(x: T): Option[T]
//  def predecessor(x: T): Option[T]
}

trait Membership[T] {
  def insert(x: T): Membership[T]
  def member(x: T): Boolean
}

