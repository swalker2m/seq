package edu.gemini.logoot

import scalaz.Scalaz._
import scalaz._

final case class Timestamp(i: Int) extends AnyVal

object Timestamp {
  implicit val Zero: Timestamp =
    Timestamp(0)

  implicit val OrderTimestamp: Order[Timestamp] =
    Order.orderBy(_.i)
}
