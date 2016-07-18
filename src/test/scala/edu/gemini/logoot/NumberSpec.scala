package edu.gemini.logoot

import org.scalacheck.Prop.{forAll, _}
import org.specs2.ScalaCheck
import org.specs2.mutable.Specification

import scalaz.Order
import scalaz.Ordering.{EQ, GT, LT}

object NumberSpec extends Specification with ScalaCheck with Arbitraries {
  "Number" should {
    "convert to/from BigInt" !
      forAll { (bi: BigInt) =>
        val abi = bi.abs
        Number.fromBigInt(abi).toBigInt == abi
      }

    "trim leading zeros" !
      forAll { (ds: List[Digit]) =>
          Number(ds).toDigits match {
          case Nil     => false
          case List(_) => true
          case h :: _  => h.toInt > 0
        }
      }

    "be ordered" !
      forAll { (n0: Number, n1: Number) =>
        val b0 = n0.toBigInt
        val b1 = n1.toBigInt

        Order[Number].order(n0, n1) match {
          case EQ => b0 == b1
          case LT => b0 <  b1
          case GT => b0 >  b1
        }
      }
  }
}
