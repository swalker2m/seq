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
      override def time: Int =
        t

      // A little more than 1 billion ticks before wrapping around.
      override def next: Timestamp =
        (t === Int.MaxValue) ? Zero | apply(t + 1)
    }

  implicit val Zero: Timestamp =
    Timestamp(0)

  implicit val Max: Timestamp =
    Timestamp(Int.MaxValue)

  implicit val OrderTimestamp: Order[Timestamp] =
    Order.orderBy(_.time)
}
