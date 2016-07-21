package edu.gemini

import scalaz._
import Scalaz._

package object logoot {
  type LogootConfig[A] = ReaderT[Id, SiteId, A]
  type LogootResult[A] = StateT[LogootConfig, LogootState, A]

  trait StateWithReader[S, C] {
    type Rd[A] = ReaderT[Id, C, A]
    type St[A] = StateT[Rd, S, A]

    def apply[A](f: (S, C) => (S, A)): St[A] =
      StateT.apply[Rd, S, A] { (s: S) =>
        ReaderT.apply[Id, C, (S, A)] { (c: C) => f(s,c) }
      }

    def get: St[S] =
      apply[S] { (s: S, c: C) => (s, s) }

    def put(s: S): St[Unit] =
      apply[Unit] { (_: S, c: C) => (s, ()) }

    def ask: St[C] =
      apply[C] { (s: S, c: C) => (s, c) }

    def modify(f: S => S): St[Unit] =
      apply[Unit] { (s: S, c: C) => (f(s), ()) }
  }

  implicit class StateWithReaderOps[S, C, A](swr: StateT[({type l[X] = ReaderT[Id, C, X]})#l, S, A]) {
    def runConfig(state: S, config: C): (S, A) =
      swr.run(state).run(config)

    def evalConfig(state: S, config: C): A =
      swr.eval(state).run(config)

    def execConfig(state: S, config: C): S =
      swr.exec(state).run(config)
  }

  object LogootResult extends StateWithReader[LogootState, SiteId] {

    /** Alias for `ask`. */
    def siteId: LogootResult[SiteId] =
      ask

    /** What time is it? */
    def now: LogootResult[Timestamp] =
      get.map(_.clock.now)

    /** Time passes. */
    def tick: LogootResult[Timestamp] =
      for {
        _ <- modify(s => LogootState.clock.set(s, s.clock.tick))
        t <- now
      } yield t
  }
}
