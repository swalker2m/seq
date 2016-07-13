package edu.gemini.model.sequence

import edu.gemini.model.ArbitraryHelpers
import edu.gemini.model.core.{OffsetQ, OffsetP, Angle}
import org.scalacheck._
import org.scalacheck.Arbitrary._

trait ArbitraryTelescope extends ArbitraryHelpers {

  val genSmallAngle: Gen[Angle] =
    Gen.choose(0.0, 100.0).map(Angle.fromArcsecs)

  implicit val arbOffsetP: Arbitrary[OffsetP] =
    Arbitrary(genSmallAngle.map(OffsetP.apply))

  implicit val arbOffsetQ: Arbitrary[OffsetQ] =
    Arbitrary(genSmallAngle.map(OffsetQ.apply))

  implicit val arbTelescope: Arbitrary[Telescope] =
    Arbitrary(
      for {
        p <- arbitrary[OffsetP]
        q <- arbitrary[OffsetQ]
      } yield Telescope(p, q)
    )
}
