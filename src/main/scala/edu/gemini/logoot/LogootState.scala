package edu.gemini.logoot

import scalaz._

final case class LogootState(site: SiteId, rng: Rng, clock: Clock)

object LogootState {
  def apply(site: SiteId, seed: Long): LogootState =
    LogootState(site, Rng(seed), Clock.Zero)

  val site: LogootState @> SiteId =
    Lens.lensu((a, b) => a.copy(site = b), _.site)

  val rng: LogootState @> Rng =
    Lens.lensu((a, b) => a.copy(rng = b), _.rng)

  val clock: LogootState @> Clock =
    Lens.lensu((a, b) => a.copy(clock = b), _.clock)
}