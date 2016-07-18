package edu.gemini.logoot

import scalaz.Scalaz._
import scalaz._

/** A `Digit` in the base-`Digit.Base` number system.
 */
sealed trait Digit {
  def toInt: Int

  override def toString: String =
    s"D$toInt"

  override def equals(a: Any): Boolean =
    a match {
      case d: Digit => d.toInt == toInt
      case _        => false
    }

  override def hashCode: Int =
    toInt
}

object Digit {
  /** The arbitrary `Base` of the number system, which must be at least > 1.
    * The larger the base, the shorter the `Position` ids.
    */
  val Base: Int =
    100

  /** Converts the integer to a `Digit` in the range (0, Base]. */
  def apply(i: Int): Digit =
    new Digit {
      val toInt: Int = (i % Base).abs
    }

  val Zero: Digit =
    Digit(0)

  /** Converts the integer to a `Digit` in the range (0, Base]. Alias for
    * `apply`.
    */
  def fromInt(i: Int): Digit =
    Digit(i)

  implicit val OrderPriority: Order[Digit] =
    Order.orderBy(_.toInt)
}