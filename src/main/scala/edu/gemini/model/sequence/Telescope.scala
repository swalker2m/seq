package edu.gemini.model.sequence

import edu.gemini.model.core.Metadata.Access.Science
import edu.gemini.model.core.Metadata.Scope.SingleStep
import edu.gemini.model.core.{Describe, TextMetadata, Prop, Metadata, OffsetQ, OffsetP}

import scalaz._

final case class Telescope(p: OffsetP, q: OffsetQ)

object Telescope {
  sealed abstract class Def(name: String) {
    val Label = Metadata.Label("Telescope")(name)
  }

  // --------------------------------------------------------------------------
  // OffsetP
  // --------------------------------------------------------------------------
  val PProperty = new Def("p") with Prop[Telescope] {
    type B = OffsetP

    val eq: Equal[OffsetP] =
      OffsetP.OrderP

    val lens: Telescope @> OffsetP =
      Lens.lensu((a,b) => a.copy(p = b), _.p)

    val meta: Metadata[OffsetP] =
      TextMetadata(Label, Science, SingleStep, Some("arcsec"))
  }

  // --------------------------------------------------------------------------
  // OffsetQ
  // --------------------------------------------------------------------------
  val QProperty = new Def("q") with Prop[Telescope] {
    type B = OffsetQ

    val eq: Equal[OffsetQ] =
      OffsetQ.OrderQ

    val lens: Telescope @> OffsetQ =
      Lens.lensu((a,b) => a.copy(q = b), _.q)

    val meta: Metadata[OffsetQ] =
      TextMetadata(Label, Science, SingleStep, Some("arcsec"))
  }

  // --------------------------------------------------------------------------
  // Describe
  // --------------------------------------------------------------------------

  implicit val DescribeTelescope: Describe[Telescope] =
    Describe.forProps(
      Telescope(OffsetP.Zero, OffsetQ.Zero),
      PProperty,
      QProperty
    )
}
