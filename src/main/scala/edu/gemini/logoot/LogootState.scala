package edu.gemini.logoot

import scalaz._
import Scalaz._

final case class LogootState(rng: Rng, clock: Clock)

object LogootState {
  val rng: LogootState @> Rng =
    Lens.lensu((a, b) => a.copy(rng = b), _.rng)

  val clock: LogootState @> Clock =
    Lens.lensu((a, b) => a.copy(clock = b), _.clock)
}