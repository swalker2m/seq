package edu.gemini.logoot

import scalaz.Ordering.{EQ, LT, GT}
import scalaz.Scalaz._
import scalaz._

sealed trait ElementId

object ElementId {
  case object Beginning                                      extends ElementId
  final case class Middle(positions: NonEmptyList[Position]) extends ElementId
  case object End                                            extends ElementId

//  def gen(p: ElementId, )

  implicit val OrderMiddle: Order[Middle] = Order.order { (m0, m1) =>
    // Add Zero positions to the end of the shorter position list. These
    // always sort before any other position.  If the two position lists share
    // the same prefix, the shorter one sorts before.
    val z = m0.positions.toList.zipAll(m1.positions.toList, Position.Zero, Position.Zero)
    z.dropWhile { case (a, b) => a == b } match {
      case Nil         => EQ
      case (a, b) :: _ => Order[Position].order(a, b)
    }
  }

  implicit val OrderElementId: Order[ElementId] =
    Order.order { (id0, id1) =>
      (id0, id1) match {
        case (m0: Middle, m1: Middle) => OrderMiddle.order(m0, m1)
        case (Beginning, Beginning)   => EQ
        case (End, End)               => EQ
        case (Beginning, _)           => LT
        case (_, Beginning)           => GT
        case (_, End)                 => LT
        case (End, _)                 => GT
      }
    }
}
