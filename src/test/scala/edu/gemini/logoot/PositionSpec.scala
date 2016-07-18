package edu.gemini.logoot

import org.scalacheck.Prop.{forAll, _}
import org.specs2.ScalaCheck
import org.specs2.mutable.Specification

import scalaz._, Scalaz._
import scalaz.Ordering.{EQ, GT, LT}

object PositionSpec extends Specification with ScalaCheck with Arbitraries {
  "Position" should {
    "be ordered" !
      forAll { (p0: Position, p1: Position) =>
        Order[Position].order(p0, p1) match {
          case EQ =>
            (p0.d == p1.d) && (p0.s == p1.s) && (p0.t == p1.t)

          case LT =>
            (p0.d < p1.d) || ((p0.d == p1.d) &&
              ((p0.s < p1.s) || ((p0.s == p1.s) && (p0.t < p1.t)))
            )

          case GT =>
            (p0.d > p1.d) || ((p0.d == p1.d) &&
              ((p0.s > p1.s) || ((p0.s == p1.s) && (p0.t > p1.t)))
            )
        }
      }

    /*
    "sort Beginning ahead of everything else" !
      forAll { (p: Position) =>
        Order[Position].order(Position.Beginning, p) match {
          case EQ => p == Position.Beginning
          case LT => true
          case GT => false
        }
      }

    "sort End after everything else" !
      forAll { (p: Position) =>
        Order[Position].order(p, Position.End) match {
          case EQ => p == Position.End
          case LT => true
          case GT => false
        }
      }
      */
  }
}
