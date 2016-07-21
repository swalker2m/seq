package edu.gemini.logoot

import scalaz._, Scalaz._

final case class Position(d: Digit, s: SiteId, t: Timestamp)

object Position {
  val Zero = Position(Digit.Zero, SiteId.Zero, Timestamp.Zero)
  val Max  = Position(Digit.Max,  SiteId.Max,  Timestamp.Max)

  def next(d: Digit): LogootResult[Position] = {
    import LogootResult._
    for {
      sid <- siteId
      t   <- tick
    } yield Position(d, sid, t)
  }

  implicit val OrderPosition: Order[Position] =
    Order.orderBy(p => (p.d, p.s, p.t))
}
