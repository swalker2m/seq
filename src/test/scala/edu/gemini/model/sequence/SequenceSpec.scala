package edu.gemini.model.sequence

import edu.gemini.model.core.Describe
import edu.gemini.model.instrument.F2
import org.scalacheck.Prop.forAll
import org.scalacheck.Prop._
import org.specs2.ScalaCheck
import org.specs2.mutable.Specification

import scalaz.NonEmptyList

object SequenceSpec extends Specification with ScalaCheck with ArbitrarySequence {

  "Sequence" should {
    "round trip" !
      forAll { (s: Sequence[F2]) =>
        Sequence.fromSteps(s.toSteps) == s
      }
  }

  def instruments[I: Describe](s: Sequence[I]): NonEmptyList[Option[I]] =
    s.toSteps.map {
      case BiasStep(i)       => i
      case DarkStep(i)       => i
      case GcalStep(i, _)    => i
      case ScienceStep(i, _) => i
      case SmartStep(i, _)   => i
    }.map(Option(_))

  def gcals[I: Describe](s: Sequence[I]): NonEmptyList[Option[GcalUnit]] =
    s.toSteps.map {
      case GcalStep(_, g) => Some(g)
      case _              => None
    }

  "SequenceLens getAll" should {
    "extract instrument values at each step" !
      forAll { (s: Sequence[F2]) =>
        val lens = SequenceLens.forInstrument(F2.Disperser.Property)
        lens.getAll(s) == instruments(s).map { _.map(_.disperser) }
      }

    "extract gcal unit values at each step" !
      forAll { (s: Sequence[F2]) =>
        val lens = SequenceLens.forGcalUnit[F2](GcalUnit.Lamp.Property)
        lens.getAll(s) == gcals(s).map { _.map(_.lamp) }
      }
  }

}
