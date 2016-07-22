package edu.gemini.logoot

import edu.gemini.logoot.ElementId.{Beginning, Middle, End}
import org.scalacheck.Prop.{forAll, _}
import org.specs2.ScalaCheck
import org.specs2.mutable.Specification

import scalaz.Ordering.{EQ, GT, LT}
import scalaz.Scalaz._
import scalaz._

object ElementIdSpec extends Specification with ScalaCheck with Arbitraries {
  "ElementId" should {
    "have Middle ordered" !
      forAll { (m0: Middle, m1: Middle) =>
        val z = m0.positions.toList.zipAll(m1.positions.toList, Position.Min, Position.Min).dropWhile {
          case (a, b) => a == b
        }

        Order[Middle].order(m0, m1) match {
          case EQ => z.isEmpty
          case LT => z.headOption.exists { case (a, b) => a < b }
          case GT => z.headOption.exists { case (a, b) => a > b }
        }
      }

    "sort Beginning ahead of everything else" !
      forAll { (id: ElementId) =>
        Order[ElementId].order(Beginning, id) match {
          case EQ => id == Beginning
          case LT => true
          case GT => false
        }
      }

    "sort End after everything else" !
      forAll { (id: ElementId) =>
        Order[ElementId].order(id, End) match {
          case EQ => id == End
          case LT => true
          case GT => false
        }
      }
  }
}
