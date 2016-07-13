package edu.gemini.model.core

trait Display[A] {
  def display(a: A): String
}

object Display {
  def apply[A](implicit instance: Display[A]): Display[A] = instance

  def instance[A](f: A => String) = new Display[A] {
    override  def display(a: A): String =
      f(a)
  }

  def instancePf[A](f: PartialFunction[A, String]) = new Display[A] {
    override def display(a: A): String =
      if (f.isDefinedAt(a)) f(a) else ""
  }

  def displayFromToString[A] = new Display[A] {
    override def display(a: A): String =
      a.toString
  }

  trait Ops[A] {
    def typeClassInstance: Display[A]
    def self: A
    def display: String = typeClassInstance.display(self)
  }

  trait ToDisplayOps {
    implicit def toDisplayOps[A](target: A)(implicit tc: Display[A]): Ops[A] = new Ops[A] {
      val self = target
      val typeClassInstance = tc
    }
  }

  object ops extends ToDisplayOps

  implicit val DisplayBoolean: Display[Boolean] =
    Display.displayFromToString[Boolean]
}
