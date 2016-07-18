package edu.gemini.logoot

import java.util.UUID

import scalaz.Scalaz._
import scalaz._

final case class SiteId(uuid: UUID) extends AnyVal

object SiteId {
  implicit val Zero: SiteId =
    SiteId(new UUID(0l, 0l))

  implicit val OrderSideId: Order[SiteId] =
    Order.orderBy { sid =>
      (sid.uuid.getMostSignificantBits, sid.uuid.getLeastSignificantBits)
    }
}
