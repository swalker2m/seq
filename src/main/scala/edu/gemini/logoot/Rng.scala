package edu.gemini.logoot

/** Random number generator from Functional Programming in Scala. */
trait Rng {
  def nextInt: (Int, Rng)

  def nextInt(max: Int): (Int, Rng) = {
    val (n, nextRng) = nextInt
    ((n % max).abs, nextRng)
  }
}

object Rng {
  def apply(seed: Long): Rng =
    new Rng {
      def nextInt: (Int, Rng) = {
        val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
        val nextRng = apply(newSeed)
        val n       = (newSeed >>> 16).toInt
        (n, nextRng)
      }
    }
}
