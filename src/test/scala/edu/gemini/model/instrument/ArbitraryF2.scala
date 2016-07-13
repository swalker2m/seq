package edu.gemini.model.instrument

import edu.gemini.model.ArbitraryHelpers
import org.scalacheck._
import org.scalacheck.Arbitrary._


/**
 *
 */
trait ArbitraryF2 extends ArbitraryHelpers {

  implicit val arbF2Disperser: Arbitrary[F2.Disperser] =
    arb(F2.Disperser.All)

  implicit val arbF2PreImaging: Arbitrary[F2.PreImaging] =
    arb(F2.PreImaging.All)

  implicit val arbF2: Arbitrary[F2] =
    Arbitrary {
      for {
        d <- arbitrary[F2.Disperser]
        p <- arbitrary[F2.PreImaging]
      } yield F2(d, p)
    }
}
