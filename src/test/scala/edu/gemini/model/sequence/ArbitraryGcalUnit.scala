package edu.gemini.model.sequence


import edu.gemini.model.ArbitraryHelpers
import org.scalacheck._
import org.scalacheck.Arbitrary._

trait ArbitraryGcalUnit extends ArbitraryHelpers {

  implicit val arbGcalUnitLamp: Arbitrary[GcalUnit.Lamp] =
    arb(GcalUnit.Lamp.All)

  implicit val arbGcalUnitShutter: Arbitrary[GcalUnit.Shutter] =
    arb(GcalUnit.Shutter.All)

  implicit val arbGcalUnit: Arbitrary[GcalUnit] =
    Arbitrary {
      for {
        l <- arbitrary[GcalUnit.Lamp]
        s <- arbitrary[GcalUnit.Shutter]
      } yield GcalUnit(l, s)
    }
}
