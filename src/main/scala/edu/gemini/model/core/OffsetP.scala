package edu.gemini.model.core

import scalaz.{\/, Order, Monoid, Show}
import scalaz.std.anyVal._

/**
 * Offset in P.
 */
case class OffsetP(toAngle: Angle) extends AnyVal

object OffsetP {
  val Zero = OffsetP(Angle.zero)

  implicit val IsoAngleP = new IsoAngle[OffsetP] {
    override def toDegrees(p: OffsetP): Double   = Angle.signedDegrees(p.toAngle.toDegrees)
    override def fromDegrees(d: Double): OffsetP = OffsetP(Angle.fromDegrees(d))
  }

  import AngleSyntax._

  implicit val ShowP: Show[OffsetP] =
    Show.shows(p => s"${p.arcsecs}")

  implicit val MonoidP: Monoid[OffsetP] =
    new Monoid[OffsetP] {
      val zero = Zero
      def append(a: OffsetP, b: => OffsetP): OffsetP = IsoAngleP.add(a, b)
    }

  implicit val OrderP: Order[OffsetP] =
    Order.orderBy(_.degrees)

  implicit val ReadP = new Read[OffsetP] {
    override def read(s: String): String \/ OffsetP =
      Read[Double].read(s).map(d => OffsetP(Angle.fromArcsecs(d)))
  }

  implicit val DisplayP: Display[OffsetP] =
    Display.instance[OffsetP](p => f"${p.arcsecs}%4.03f arcsecs")
}
