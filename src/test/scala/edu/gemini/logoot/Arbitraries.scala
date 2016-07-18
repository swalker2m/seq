package edu.gemini.logoot

import org.scalacheck.Arbitrary._
import org.scalacheck._

import java.util.UUID

import scalaz.NonEmptyList

trait Arbitraries {
  implicit val arbDigit: Arbitrary[Digit] =
    Arbitrary {
      Gen.chooseNum(0, Digit.Base - 1).map(Digit.fromInt)
    }

  implicit val arbNumber: Arbitrary[Number] =
    Arbitrary {
      arbitrary[List[Digit]].map(Number(_))
    }

  implicit val arbTimestamp: Arbitrary[Timestamp] =
    Arbitrary {
      arbitrary[Int].map(Timestamp(_))
    }

  implicit val arbSiteId: Arbitrary[SiteId] =
    Arbitrary {
      for {
        msb <- arbitrary[Long]
        lsb <- arbitrary[Long]
      } yield SiteId(new UUID(msb, lsb))
    }

  implicit val arbPosition: Arbitrary[Position] =
    Arbitrary {
      for {
        d <- arbitrary[Digit]
        s <- arbitrary[SiteId]
        t <- arbitrary[Timestamp]
      } yield Position(d, s, t)
    }

  import ElementId.{Beginning, Middle, End}

  implicit val arbMiddle: Arbitrary[Middle] =
    Arbitrary {
      for {
        h <- arbitrary[Position]
        t <- arbitrary[List[Position]]
      } yield Middle(NonEmptyList[Position](h, t: _*))
    }

  implicit val arbElementId: Arbitrary[ElementId] =
    Arbitrary {
      Gen.frequency[ElementId](
         1 -> Beginning,
        98 -> arbitrary[Middle],
         1 -> End)
    }
}
