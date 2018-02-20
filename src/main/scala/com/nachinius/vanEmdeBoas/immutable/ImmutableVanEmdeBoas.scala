package com.nachinius.vanEmdeBoas.immutable

import com.nachinius.vanEmdeBoas.{Lower, Upper, vanEmdeBoas}


object ImmutableVanEmdeBoas {
  /**
    * @param bits Bits used to store the numbers (w = log u). Numbers allowed will be in range
    *             [0,1,...,2^bits-1]
    **/
  def apply[T](bits: Int): ImmutableVanEmdeBoas = Empty(bits)

}

sealed trait ImmutableVanEmdeBoas extends vanEmdeBoas {
  val bits: Int

  val maxNumber = (1 << bits) - 1
  val minNumber = 0
  val halfbits: Int = math.ceil(0.5 * bits.toDouble).intValue()
  val lowerbits: Int = bits - halfbits
  require(halfbits + lowerbits == bits, "inconsistency calculate for bits sum")
  require(maxNumber > 0, s"maxNumber should be positive, current $maxNumber for $bits, $halfbits, $lowerbits")

  override def insert(x: T): vanEmdeBoas

  override def delete(x: T): vanEmdeBoas = this
}

case class Main[S <: vanEmdeBoas](
                                   bits: Int,
                                   min: Int,
                                   max: Int,
                                   summary: S,
                                   clusters: Map[Upper, S]) extends ImmutableVanEmdeBoas {
  override def toString(): String = {
    toVector.sorted.map(_.toString()).mkString(", ")
  }

  override def foreach[U](f: Int => U): Unit = {
    f(min)
    clusters.foreach {
      case (Upper(v), s) => s.foreach(
        x => f(v * (1 << lowerbits) + x)
      )
    }
  }

  override def insert(x: T) = {
    if (x > maxNumber) {
      val message = s"$x is out of bounds (max=$maxNumber) from bits=$bits"
      throw new IllegalArgumentException("requirement failed: " + message)
    }
    if (x == min) {
      this
    } else {
      val (nextMin, nextInsert) = if (x < min) (x, min) else (min, x)
      val nextMax = if (x > max) x else max

      val (nextSummary, nextClusters) = calculateSummaryAndCluster(nextInsert)

      Main(bits, nextMin, nextMax, nextSummary, nextClusters)
    }

  }

  private[this] def calculateSummaryAndCluster(x: Int) = {
    val (c, i) = expr(x)
    clusters.get(c) match {
      case None =>
        (summary.insert(c.value),
          clusters + (c -> Empty(lowerbits).insert(i.value)))
      case Some(cluster) =>
        (summary,
          clusters.updated(c, cluster.insert(i.value)))
    }
  }

  override def successor(x: Int): Option[Int] =
    if (x < min) Some(min)
    else {
      val (c, l) = expr(x)
      getSuccessorInsideCluster(c, l) orElse
        getSuccessorFromSummary(c, l)
    }

  private def getSuccessorInsideCluster(c: Upper, l: Lower) = {
    for {
      cluster <- clusters.get(c) if l.value < cluster.max
      next <- cluster.successor(l.value)
    } yield toNumber(c, Lower(next))
  }

  private def getSuccessorFromSummary(upper: Upper, lower: Lower): Option[Int] = {
    for {
      idx <- summary.successor(upper.value)
      cluster <- clusters.get(Upper(idx))
      value <- cluster match {
        case Main(_, m, _, _, _) => Some(m)
        case SingleBit(m, _, _) => Some(m)
        case _ => None
      }
    } yield toNumber(Upper(idx), Lower(value))
  }

  override def member(x: Int): Boolean = {
    if (min == x || max == x) true
    else if (x < max) {
      val (c, l) = expr(x)
      clusters.get(c) match {
        case None => false
        case Some(s) => s.member(l.value)
      }
    } else false
  }

  override def predecessor(x: T): Option[T] = {
    if (x > max) Some(max)
    else if (x <= min) None
    else {
      val (c, l) = expr(x)
      getPredecessorInsideCluster(c, l) orElse getPredecessorFromSummary(c, l) orElse Some(min)
    }
  }

  def getPredecessorInsideCluster(c: Upper, l: Lower): Option[T] = for {
    cluster <- clusters.get(c) if l.value > cluster.min
    next <- cluster.predecessor(l.value)
  } yield toNumber(c, Lower(next))

  def getPredecessorFromSummary(upper: Upper, l: Lower): Option[T] = for {
    idx <- summary.predecessor(upper.value)
    cluster <- clusters.get(Upper(idx))
    value <- cluster match {
      case Main(_, _, mx, _, _) => Some(mx)
      case SingleBit(_, mx, _) => Some(mx)
      case _ => None
    }
  } yield toNumber(Upper(idx), Lower(value))
}

/**
  * Represents an empty van Emde Boas (and thus, with no summary, and no clusters)
  *
  * @param bits
  */
case class Empty(bits: Int) extends ImmutableVanEmdeBoas {
  override def insert(x: Int): ImmutableVanEmdeBoas =
    if (bits == 1) SingleBit(x, x)
    else Main(bits, x, x, Empty(halfbits), Map())

  override def predecessor(x: T): Option[T] = None

  override def successor(x: Int): Option[Int] = None

  override def member(x: Int): Boolean = false

  override def foreach[U](f: Int => U): Unit = ()
}

case class SingleBit(min: Int, max: Int, bits: Int = 1) extends ImmutableVanEmdeBoas {
  self =>

  override def insert(x: Int): ImmutableVanEmdeBoas =
    if (x < min || x > max) this.copy(0, 1)
    else this

  //    case (0,1,1) => this.copy(0,1)
  //    case (1,0,0) => this.copy(0,1)
  //    case (0,0,0) => this
  //    case (0,0,1) => this
  //    case (1,0,1) => this
  //    case (1,1,1) => this

  override def predecessor(x: T): Option[T] =
    if (x == 1 && min == 0) Some(0) else None

  override def successor(x: Int): Option[Int] = {
    if (x == 0 && max == 1) Some(1) else None
  }

  override def member(x: Int): Boolean = x == min || x == max

  override def foreach[U](f: Int => U): Unit = {
    if (min != max) {
      f(min)
      f(max)
    } else f(min)
  }
}
