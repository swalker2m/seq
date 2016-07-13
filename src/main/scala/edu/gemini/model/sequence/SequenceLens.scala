package edu.gemini.model.sequence

import edu.gemini.model.core.{Prop, Describe}

import scalaz.{NonEmptyList, @?>}

/** Convenience for working with `Sequence`.
  *
  * TODO: I'm not sure how useful these are beyond serving as an example.
  */
final case class SequenceLens[A: Describe, I: Describe](prop: Prop[A], aLens: Step[I] @?> A) {
  val lens: Step[I] @?> prop.B = aLens >=> prop.lens.partial

  /** Gets all values, one per step.  If the step type doesn't support the
    * property, the corresponding list element will be empty.
    */
  def getAll(seq: Sequence[I]): NonEmptyList[Option[prop.B]] =
    seq.toSteps.map(lens.get)

  /** Sets the property value for all steps that support the property, leaving
    * those that do not unchanged.
    */
  def setAll(seq: Sequence[I], b: prop.B): Sequence[I] =
    Sequence.fromSteps(seq.toSteps.map(s => lens.set(s, b).getOrElse(s)))

  /** Gets the common value if it is the same across all steps that support the
    * property.  If two or more property supporting steps have different values
    * then None.
    */
  def getCommon(seq: Sequence[I]): Option[prop.B] =
    getAll(seq).list.toList.flatten.distinct match {
      case List(b) => Some(b)
      case _       => None
    }
}

object SequenceLens {
  def forGcalUnit[I: Describe](p: Prop[GcalUnit]): SequenceLens[GcalUnit, I] =
    SequenceLens(p, Step.gcal[I])

  def forInstrument[I: Describe](p: Prop[I]): SequenceLens[I, I] =
    SequenceLens(p, Step.instrument[I])

  def forSmartCal[I: Describe](p: Prop[SmartCal]): SequenceLens[SmartCal, I] =
    SequenceLens(p, Step.smartCal[I])

  def forTelescope[I: Describe](p: Prop[Telescope]): SequenceLens[Telescope, I] =
    SequenceLens(p, Step.telescope[I])
}
