package edu.gemini.model.core

import scalaz.{\/, Order, Monoid, Show}
import scalaz.std.anyVal._

/**
 * Offset in Q.
 */
case class OffsetQ(toAngle: Angle) extends AnyVal

object OffsetQ {
  val Zero = OffsetQ(Angle.zero)

  implicit val IsoAngleQ = new IsoAngle[OffsetQ] {
    override def toDegrees(q: OffsetQ): Double   = Angle.signedDegrees(q.toAngle.toDegrees)
    override def fromDegrees(d: Double): OffsetQ = OffsetQ(Angle.fromDegrees(d))
  }

  import AngleSyntax._

  implicit val ShowQ: Show[OffsetQ] =
    Show.shows(q => f"${q.arcsecs}%4.03f")

  implicit val MonoidQ: Monoid[OffsetQ] =
    new Monoid[OffsetQ] {
      val zero = Zero
      def append(a: OffsetQ, b: => OffsetQ): OffsetQ = IsoAngleQ.add(a, b)
    }

  implicit val OrderQ: Order[OffsetQ] =
    Order.orderBy(_.degrees)

  implicit val ReadQ = new Read[OffsetQ] {
    override def read(s: String): String \/ OffsetQ =
      Read[Double].read(s).map(d => OffsetQ(Angle.fromArcsecs(d)))
  }

  implicit val DisplayQ: Display[OffsetQ] =
    Display.instance[OffsetQ](q => f"${q.arcsecs}%4.03f arcsecs")
}
