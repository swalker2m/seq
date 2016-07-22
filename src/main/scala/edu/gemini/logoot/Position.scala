package edu.gemini.logoot

import scalaz._, Scalaz._

final case class Position(d: Digit, s: SiteId, t: Timestamp)

object Position {
  val Min = Position(Digit.Zero, SiteId.Min, Timestamp.Zero)
  val Max = Position(Digit.Max,  SiteId.Max, Timestamp.Max)

  def next(d: Digit): Logoot[Position] = {
    import LogootOp._
    for {
      sid <- site
      _   <- tick
      t   <- timeNow
    } yield Position(d, sid, t)
  }

  implicit val OrderPosition: Order[Position] =
    Order.orderBy(p => (p.d, p.s, p.t))
}
