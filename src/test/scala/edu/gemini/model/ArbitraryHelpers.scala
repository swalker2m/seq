package edu.gemini.model

import org.scalacheck.{Arbitrary, Gen}

import scalaz.NonEmptyList

/**
 *
 */
trait ArbitraryHelpers {
  def oneOf[A](as: NonEmptyList[A]): Gen[A] =
    Gen.oneOf(as.list.toList)

  def arb[A](as: NonEmptyList[A]): Arbitrary[A] =
    Arbitrary(oneOf(as))
}
