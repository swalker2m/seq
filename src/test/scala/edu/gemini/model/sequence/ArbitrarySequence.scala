package edu.gemini.model.sequence

import edu.gemini.model.core.Describe
import edu.gemini.model.instrument.F2
import org.scalacheck._
import org.scalacheck.Arbitrary._
import org.scalacheck.Gen._

import scalaz.{IList, NonEmptyList}

trait ArbitrarySequence extends edu.gemini.model.instrument.Arbitraries with ArbitraryGcalUnit with ArbitraryTelescope {

  implicit val arbSmartCalType: Arbitrary[SmartCal.Type] =
    arb(SmartCal.Type.All)

  implicit val arbSmartCal: Arbitrary[SmartCal] =
    Arbitrary(arbitrary[SmartCal.Type].map { t => SmartCal(t) })

  implicit val arStepType: Arbitrary[Step.Type] =
    Arbitrary(Gen.oneOf(Step.Bias, Step.Dark, Step.Gcal, Step.Science, Step.Smart))

  def arbStepOfType[I : Arbitrary](t: Step.Type): Arbitrary[Step[I]] = {
    def genStep(t: Step.Type, i: I): Gen[Step[I]] =
      t match {
        case Step.Bias    => BiasStep(i)
        case Step.Dark    => DarkStep(i)
        case Step.Gcal    => arbitrary[GcalUnit].map(GcalStep(i, _))
        case Step.Science => arbitrary[Telescope].map(ScienceStep(i, _))
        case Step.Smart   => arbitrary[SmartCal].map(SmartStep(i, _))
      }

    Arbitrary(
      for {
        i <- arbitrary[I]
        s <- genStep(t, i)
      } yield s
    )
  }

  def arbStep[I : Arbitrary]: Arbitrary[Step[I]] =
    Arbitrary(
      for {
        t <- arbitrary[Step.Type]
        s <- arbStepOfType[I](t).arbitrary
      } yield s
    )

  def arbSequenceI[I : Arbitrary : Describe]: Arbitrary[Sequence[I]] =
    Arbitrary(
      for {
        h <- arbStep[I].arbitrary
        n <- choose(0, 99)
        t <- listOfN(n, arbStep[I].arbitrary)
        steps = NonEmptyList.nel[Step[I]](h, IList.fromList(t))
      } yield Sequence.fromSteps(steps)
    )

  // TODO: choose a random instrument and make an Arbitrary[Sequence[_]] ?
  implicit val arbSequence: Arbitrary[Sequence[F2]] =
    arbSequenceI[F2]
}
