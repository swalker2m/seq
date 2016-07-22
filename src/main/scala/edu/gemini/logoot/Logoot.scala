package edu.gemini.logoot

import scalaz._

import LogootOp._

object Logoot {
  private object xform {
    type Result[A] = State[LogootState, A]

    val interp: LogootOp ~> Result =
      new (LogootOp ~> Result) {
        def apply[A](op: LogootOp[A]): Result[A] =
          op match {
            case Site    => LogootState.site.st
            case TimeNow => LogootState.clock.map(_.now)
            case Tick    => LogootState.clock.mods_(_.tick)
            case Rand(m) =>
              for {
                r1     <- LogootState.rng.st
                (r2, i) = r1.nextInt(m)
                _      <- LogootState.rng.assign(r2)
              } yield i
          }
      }
  }

  def eval[A](prog: Logoot[A], siteId: SiteId, seed: Long): A =
    eval(prog, LogootState(siteId, seed))

  def eval[A](prog: Logoot[A], s: LogootState): A =
    run(prog, s)._2

  def run[A](prog: Logoot[A], siteId: SiteId, seed: Long): (LogootState, A) =
    run(prog, LogootState(siteId, seed))

  def run[A](prog: Logoot[A], s: LogootState): (LogootState, A) =
    prog.foldMap(xform.interp).run(s)
}
