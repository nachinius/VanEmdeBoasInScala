package com.nachinius.vanEmdeBoas

/**
  * Basically is a Set
  * @tparam T
  */
trait Membership[T] {

  /** Insert an element into this structure **/
  def insert(x: T): Membership[T]

  /** Queries if an element exists in this structure **/
  def member(x: T): Boolean
}
