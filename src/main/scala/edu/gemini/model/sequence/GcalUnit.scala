package edu.gemini.model.sequence

import edu.gemini.model.core.Metadata.Access.Science
import edu.gemini.model.core.Metadata.Scope.SingleStep
import edu.gemini.model.core.{Describe, Display, EnumMetadata, Metadata, Prop, Read}
import edu.gemini.model.sequence.GcalUnit.LampType.{Arc, Flat}
import edu.gemini.model.sequence.GcalUnit.{Lamp, Shutter}

import scalaz._

final case class GcalUnit(lamp: Lamp, shutter: Shutter)

object GcalUnit {

  sealed abstract class Def(name: String) {
    val Label = Metadata.Label("GCal Unit")(name)
  }

  // --------------------------------------------------------------------------
  // Lamp
  // --------------------------------------------------------------------------
  sealed trait LampType

  object LampType {
    case object Arc extends LampType
    case object Flat extends LampType
  }

  sealed abstract class Lamp(lampType: LampType)

  object Lamp extends Def("Lamp") {
    case object IrGreyBodyHigh extends Lamp(Flat)
    case object IrGreyBodyLow  extends Lamp(Flat)
    case object Quartz         extends Lamp(Flat)
    case object ArArc          extends Lamp(Arc)
    case object ThArArc        extends Lamp(Arc)
    case object CuArArc        extends Lamp(Arc)
    case object XeArc          extends Lamp(Arc)

    val Default = IrGreyBodyHigh
    val All     = NonEmptyList[Lamp](IrGreyBodyHigh, IrGreyBodyLow, Quartz, ArArc, ThArArc, CuArArc, XeArc)

    implicit val EqualLamp   = Equal.equalA[Lamp]
    implicit val ShowLamp    = Show.showFromToString[Lamp]
    implicit val ReadLamp    = Read.fromNel(All)
    implicit val DisplayLamp = Display.instance[Lamp] {
      case IrGreyBodyHigh => "IR grey body - high"
      case IrGreyBodyLow  => "IR grey body - low"
      case Quartz         => "Quartz Hologen"
      case ArArc          => "Ar arc"
      case ThArArc        => "ThAr arc"
      case CuArArc        => "CuAr arc"
      case XeArc          => "Xe arc"
    }

    val Property = new Prop[GcalUnit] {
      type B = Lamp

      val eq: Equal[Lamp] =
        EqualLamp

      val lens: GcalUnit @> Lamp =
        Lens.lensu((a, b) => a.copy(lamp = b), _.lamp)

      val meta: Metadata[Lamp] =
        EnumMetadata(Label, Science, SingleStep, All)
    }
  }

  // --------------------------------------------------------------------------
  // Shutter
  // --------------------------------------------------------------------------
  sealed trait Shutter

  object Shutter extends Def("Shutter") {
    case object Open extends Shutter
    case object Closed extends Shutter

    val Default = Open
    val All     = NonEmptyList[Shutter](Open, Closed)

    implicit val EqualShutter   = Equal.equalA[Shutter]
    implicit val ShowShutter    = Show.showFromToString[Shutter]
    implicit val ReadShutter    = Read.fromNel(All)
    implicit val DisplayShutter = Display.displayFromToString[Shutter]

    val Property = new Prop[GcalUnit] {
      type B = Shutter

      val eq: Equal[Shutter] =
        EqualShutter

      val lens: GcalUnit @> Shutter =
        Lens.lensu((a, b) => a.copy(shutter = b), _.shutter)

      val meta: Metadata[Shutter] =
        EnumMetadata(Label, Science, SingleStep, All)
    }
  }

  // --------------------------------------------------------------------------
  // Describe
  // --------------------------------------------------------------------------

  implicit val DescribeGcalUnit: Describe[GcalUnit] =
    Describe.forProps(
      GcalUnit(Lamp.Default, Shutter.Default),
      Lamp.Property,
      Shutter.Property
    )
}