package edu.gemini.logoot

import scalaz.Scalaz._
import scalaz._

/** An arbitrary length unsigned number in base `Digit.Base`. */
sealed trait Number {

  /** Gets the individual digits that make up this number. */
  def toDigits: List[Digit]

  /** Calculates the base-10 `BigInt` equivalent of this `Number`. */
  def toBigInt: BigInt =
    (toDigits:\(BigInt(0), BigInt(1))) { case (d, (acc, pow)) =>
      (acc + BigInt(d.toInt) * pow, pow * Digit.Base)
    }._1
}

object Number {
  def apply(ds: List[Digit]): Number =
    new Number {
      def toDigits: List[Digit] =
        ds.dropWhile(_.toInt == 0) match {
          case Nil => List(Digit.Zero)
          case x   => x
        }
    }

  /** Calculates the `Number` in `Digit.Base` that corresponds to the given
    * base-10 `BitInt`.
    *
    * If `bi` is negative, the absolute value is used instead.
    */
  def fromBigInt(bi: BigInt): Number = {
    val zero = BigInt(0)

    def go(rem: BigInt, res: List[Digit]): List[Digit] = {
      val (a, b) = rem /% Digit.Base
      val res0   = Digit.fromInt(b.toInt) :: res
      if (a == zero) res0 else go(a, res0)
    }

    Number(go(bi.abs, Nil))
  }

  implicit val OrderNumber: Order[Number] = Order.orderBy(_.toBigInt)
}
