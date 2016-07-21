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

    val initialState = LogootState(Rng(0), Clock.Zero)

    "advance clock when getting the next position id" !
      forAll { (digits: List[Digit], sid: SiteId) =>
        val (state, posList) = digits.map(Position.next).sequenceU.runConfig(initialState, sid)

        (state.clock.now === Timestamp(digits.length)) &&
          posList.forall(_.s === sid) &&
          posList.map(_.t.time) == posList.zipWithIndex.unzip._2.map(_ + 1) &&
          posList.map(_.d) == digits
      }
  }
}
