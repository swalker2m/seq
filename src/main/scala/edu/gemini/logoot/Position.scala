package edu.gemini.logoot

import scalaz.Scalaz._
import scalaz._

final case class Position(d: Digit, s: SiteId, t: Timestamp)

object Position {
  val Zero = Position(Digit.Zero, SiteId.Zero, Timestamp.Zero)

  implicit val OrderPosition: Order[Position] =
    Order.orderBy(p => (p.d, p.s, p.t))
}
