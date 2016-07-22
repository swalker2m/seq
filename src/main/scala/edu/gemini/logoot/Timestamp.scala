package edu.gemini.logoot

import scalaz.Scalaz._
import scalaz._

sealed trait Timestamp {
  def time: Int
  def next: Timestamp
}

object Timestamp {
  def apply(t: Int): Timestamp =
    new Timestamp {
      override val time: Int =
        if (t < 0) 0 else t

      // A little more than 1 billion ticks before wrapping around.
      override def next: Timestamp =
        apply(time + 1)
    }

  val Zero: Timestamp =
    Timestamp(0)

  val Max: Timestamp =
    Timestamp(Int.MaxValue)

  implicit val OrderTimestamp: Order[Timestamp] =
    Order.orderBy(_.time)
}
