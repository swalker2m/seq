package edu.gemini.logoot

/** Random number generator from Functional Programming in Scala. */
trait Rng {
  def nextInt: (Rng, Int)

  def nextInt(max: Int): (Rng, Int) = {
    val (nextRng, n) = nextInt
    (nextRng, (n % max).abs)
  }
}

object Rng {
  def apply(seed: Long): Rng =
    new Rng {
      def nextInt: (Rng, Int) = {
        val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
        (apply(newSeed), (newSeed >>> 16).toInt)
      }
    }
}
