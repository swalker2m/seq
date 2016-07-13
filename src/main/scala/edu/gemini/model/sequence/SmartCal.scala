package edu.gemini.model.sequence

import edu.gemini.model.core.Metadata.Access.Science
import edu.gemini.model.core.Metadata.Scope.SingleStep
import edu.gemini.model.core.{Describe, EnumMetadata, Prop, Display, Read, Metadata}

import scalaz.{Lens, @>, Show, Equal, NonEmptyList}

final case class SmartCal(smartCalType: SmartCal.Type)

object SmartCal {

  sealed abstract class Def(name: String) {
    val Label = Metadata.Label("SmartCal")(name)
  }

  // --------------------------------------------------------------------------
  // Type
  // --------------------------------------------------------------------------
  sealed trait Type

  object Type extends Def("Type") {
    case object Arc           extends Type
    case object Flat          extends Type
    case object DayBaseline   extends Type
    case object NightBaseline extends Type

    val Default = Arc
    val All     = NonEmptyList[Type](Arc, Flat, DayBaseline, NightBaseline)

    implicit val EqualType   = Equal.equalA[Type]
    implicit val ShowType    = Show.showFromToString[Type]
    implicit val ReadType    = Read.fromNel(All)
    implicit val DisplayType = Display.instance[Type] {
      case Arc           => "Arc"
      case Flat          => "Flat"
      case DayBaseline   => "Day Baseline"
      case NightBaseline => "Night Baseline"
    }

    val Property = new Prop[SmartCal] {
      type B = Type

      val eq: Equal[Type] =
        EqualType

      val lens: SmartCal @> Type =
        Lens.lensu((a,b) => a.copy(smartCalType = b), _.smartCalType)

      val meta: Metadata[Type] =
        EnumMetadata(Label, Science, SingleStep, All)
    }
  }


  // --------------------------------------------------------------------------
  // Describe
  // --------------------------------------------------------------------------

  implicit val DescribeSmartCal: Describe[SmartCal] =
    Describe.forProps(
      SmartCal(Type.Default),
      Type.Property
    )
}