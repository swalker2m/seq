package edu.gemini.logoot

sealed trait Clock {
  def now: Timestamp
  def tick: Clock
}

object Clock {
  val Zero: Clock =
    Clock(Timestamp.Zero)

  def apply(ts: Timestamp): Clock =
    new Clock {
      def now: Timestamp =
        ts

      def tick: Clock =
        apply(ts.next)
    }
}
