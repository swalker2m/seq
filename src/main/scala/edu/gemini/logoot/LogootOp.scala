package edu.gemini.logoot


import scalaz._

sealed trait LogootOp[A]

object LogootOp {
  case object Site          extends LogootOp[SiteId]
  case object TimeNow       extends LogootOp[Timestamp]
  case object Tick          extends LogootOp[Unit]
  case class Rand(max: Int) extends LogootOp[Int]

  val site         = Free.liftF(Site)
  val timeNow      = Free.liftF(TimeNow)
  val tick         = Free.liftF(Tick)
  def rand(m: Int) = Free.liftF(Rand(m))
}
