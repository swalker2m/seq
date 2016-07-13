package edu.gemini.model.core

import scalaz.{\/, Show}

trait PropValue[A] extends Show[A] with Read[A] with Display[A]

object PropValue {
  implicit def propValue[A](implicit s: Show[A], r: Read[A], d: Display[A]): PropValue[A] =
    new PropValue[A] {
      override def shows(a: A): String =
        s.shows(a)

      override def read(s: String): String \/ A =
        r.read(s)

      override def display(a: A): String =
        d.display(a)
    }
}