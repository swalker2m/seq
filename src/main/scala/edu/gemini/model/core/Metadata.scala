package edu.gemini.model.core

import Metadata.{Access, Label, Scope}

import scalaz._
import Scalaz._

/** Metadata describing the values of a property. */
sealed abstract class Metadata[A](implicit pv: PropValue[A]) {
  def label: Label
  def access: Access
  def scope: Scope

  // Record the propValue instance so we can refer to it when working with an
  // arbitrary Metadata[A].

  def propValue: PropValue[A] = pv
}

object Metadata {
  case class Label(parent: Option[Label], name: String) {

    def apply(n: String): Label =
      Label(this, n)
  }

  object Label {
    def apply(name: String): Label = Label(none, name)

    def apply(parent: Label, name: String): Label = Label(some(parent), name)

    implicit val ShowLabel: Show[Label] = Show.shows { l =>
      l.parent.fold(l.name)(p => s"${p.shows} / ${l.name}")
    }

    implicit val OrderLabel: Order[Label] = Order[String].contramap(_.shows)

    implicit val OrderingLabel: scala.Ordering[Label] = OrderLabel.toScalaOrdering
  }


  sealed trait Access
  object Access {
    case object Engineering extends Access
    case object Science     extends Access
  }

  sealed trait Scope
  object Scope {
    case object Global     extends Scope
    case object SingleStep extends Scope
  }
}

/** Metadata for properties with a list of possible values.  The idea is that
  * these will be edited with a combo box widget. */
final case class EnumMetadata[A: PropValue](
   label: Label,
   access: Access,
   scope: Scope,
   values: NonEmptyList[A]) extends Metadata[A]

/** Metadata for properties with two values, one that can be interpreted as
  * true and the other false.  These can be edited with check boxes. Note,
  * there is no requirement that "boolean" properties actually have underlying
  * type Boolean. */
final case class BooleanMetadata[A: PropValue](
    label: Label,
    access: Access,
    scope: Scope,
    t: A,
    f: A) extends Metadata[A]

object BooleanMetadata {

  /** Support for creating BooleanMetadata for properties with underlying type
    * Boolean. */
  def forBoolean(label: Label, access: Access, scope: Scope): Metadata[Boolean] =
    BooleanMetadata[Boolean](label, access, scope, true, false)
}

/** Metadata for properties of arbitrary type that can be represented with a
  * String value.  These properties can be edited with a text field. */
final case class TextMetadata[A: PropValue](
  label: Label,
  access: Access,
  scope: Scope,
  units: Option[String]) extends Metadata[A]