package edu.gemini.model.core

import scalaz._
import Scalaz._

trait Read[A] {
  def read(s: String): String \/ A
}

object Read {
  def apply[A](implicit instance: Read[A]): Read[A] = instance

  def fromNel[A: Show : Manifest](as: NonEmptyList[A]): Read[A] =
    fromIList(as.list)

  def fromIList[A: Show : Manifest](as: IList[A]): Read[A] =
    fromList(as.toList)

  def fromList[A: Show : Manifest](as: List[A]) = new Read[A] {
    def read(s: String): String \/ A = {
      val m = implicitly[Manifest[A]]
      as.find(_.shows === s) \/> s"Could not parse '$s' as ${m.runtimeClass.getName}"
    }
  }

  implicit val ReadBoolean: Read[Boolean] = new Read[Boolean] {
    override def read(s: String): String \/ Boolean =
      \/.fromTryCatchNonFatal(s.toBoolean).leftMap(_ => s"Could not parse '$s' as a Boolean")
  }

  implicit val ReadDouble: Read[Double] = new Read[Double] {
    override def read(s: String): String \/ Double =
      \/.fromTryCatchNonFatal(s.toDouble).leftMap(_ => s"Could not parse '$s' as a Double")
  }
}
