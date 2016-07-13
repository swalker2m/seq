package edu.gemini.model.instrument

import edu.gemini.model.core.Metadata.Access._
import edu.gemini.model.core.Metadata.Scope._
import edu.gemini.model.core.{Describe, BooleanMetadata, Display, EnumMetadata, Metadata, Prop, Read}

import scalaz._

import F2._

case class F2(
  disperser: Disperser,
  preImaging: PreImaging
)

object F2 {
//  final case class Static(preImaging: PreImaging)
//  final case class Dynamic(disperser: Disperser /*, filter: Filter etc*/)
//  final case class StepConfig(s: Static, d: Dynamic)

  sealed abstract class Def(name: String) {
    val Label = Metadata.Label("F2")(name)
  }

  // --------------------------------------------------------------------------
  // Disperser
  // --------------------------------------------------------------------------
  sealed trait Disperser

  object Disperser extends Def("Disperser") {
    case object None    extends Disperser
    case object R1200JH extends Disperser
    case object R1200HK extends Disperser
    case object R3000   extends Disperser

    val Default = None
    val All     = NonEmptyList[Disperser](None, R1200JH, R1200HK, R3000)

    implicit val EqualDisperser   = Equal.equalA[Disperser]
    implicit val ShowDisperser    = Show.showFromToString[Disperser]
    implicit val ReadDisperser    = Read.fromNel(All)
    implicit val DisplayDisperser = Display.instance[Disperser] {
      case None    => "None"
      case R1200JH => "R=1200 (J + H) grism"
      case R1200HK => "R=1200 (H + K) grism"
      case R3000   => "R=3000 (J or H or K) grism"
    }

    val Property = new Prop[F2] {
      type B = Disperser

      val eq: Equal[Disperser]  =
        EqualDisperser

      val lens: F2 @> Disperser =
        Lens.lensu((a,b) => a.copy(disperser = b), _.disperser)

      val meta: Metadata[Disperser] =
        EnumMetadata(Label, Science, SingleStep, All)
    }
  }


  // --------------------------------------------------------------------------
  // Pre-imaging Flag
  // --------------------------------------------------------------------------
  sealed trait PreImaging

  object PreImaging extends Def("MOS Pre-Imaging") {
    case object T extends PreImaging
    case object F extends PreImaging

    val Default = F
    val All     = NonEmptyList[PreImaging](T, F)

    implicit val EqualPreImaging   = Equal.equalA[PreImaging]
    implicit val ShowPreImaging    = Show.showFromToString[PreImaging]
    implicit val ReadPreImaging    = Read.fromNel(All)
    implicit val DisplayPreImaging = Display.instance[PreImaging] {
      case T => "T"
      case F => "F"
    }

    val Property = new Prop[F2] {
      type B = PreImaging

      val eq: Equal[PreImaging] =
        EqualPreImaging

      val lens: F2 @> PreImaging =
        Lens.lensu((a, b) => a.copy(preImaging = b), _.preImaging)

      val meta: Metadata[PreImaging] =
        BooleanMetadata(Label, Science, Global, T, F)
    }
  }

  // --------------------------------------------------------------------------
  // Describe
  // --------------------------------------------------------------------------

  implicit val DescribeF2: Describe[F2] =
    Describe.forProps(
      F2(Disperser.Default, PreImaging.Default),
      Disperser.Property,
      PreImaging.Property
    )

//  implicit val DescribeStatic: Describe[Static] =
//    Describe.forProps(
//      Static(PreImaging.Default),
//      PreImaging.Property
//    )
//
//  implicit val DescribeDynamic: Describe[Dynamic] =
//    Describe.forProps(
//      Dynamic(Disperser.Default),
//      Disperser.Property
//    )
//
//  implicit val DescribeStepConfig: Describe[StepConfig] =
//    Describe.describe2(DescribeStatic, DescribeDynamic).xmap(
//      { case (s,d) => StepConfig(s, d) },
//      sc => (sc.s, sc.d)
//    )
}
