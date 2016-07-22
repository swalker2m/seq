package edu.gemini.logoot


import scalaz._

sealed trait LogootOp[A]

object LogootOp {
  case object Site           extends LogootOp[SiteId]
  case object TimeNow        extends LogootOp[Timestamp]
  case object Tick           extends LogootOp[Unit]
  case class Rand(n: Number) extends LogootOp[Number]

  val site            = Free.liftF(Site)
  val timeNow         = Free.liftF(TimeNow)
  val tick            = Free.liftF(Tick)
  def rand(n: Number) = Free.liftF(Rand(n))
}
