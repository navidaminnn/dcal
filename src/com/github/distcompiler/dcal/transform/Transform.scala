package com.github.distcompiler.dcal.transform

import cats.*
import cats.syntax.all.given
import com.github.distcompiler.dcal.util.{SummonFallback, SummonTuple}

opaque type Transform[A, B] = A => B

object Transform {
  opaque type Lazy[A, B] = Transform[A, B]

  object Lazy {
    given instance[A, B](using fn: =>Transform[A, B]): Lazy[A, B] =
      from => fn(from)
  }

  extension [A, B](self: Lazy[A, B]) {
    def asTransform: Transform[A, B] = self
  }

  def fromFunction[A, B](fn: A => B): Transform[A, B] = fn

  extension [A, B](self: Transform[A, B]) {
    def asFunction: A => B = self

    def apply(from: A): B = (self: A => B).apply(from)
  }

  given refinedOrGeneric[A, B](using choice: SummonFallback[Refined[A, B], Generic[A, B]]): Transform[A, B] =
    choice.value match {
      case Left(generic) => generic
      case Right(refined) => refined
    }

  opaque type Generic[A, B] = Transform[A, B]

  object Generic {
    def fromFunction[A, B](fn: A => B): Generic[A, B] = fn

    extension [A, B](self: Generic[A, B]) {
      def apply(from: A): B = self(from)

      def asFunction: A => B = self
    }
  }

  opaque type Refined[A, B] = Transform[A, B]

  object Refined {
    extension [A, B](self: Refined[A, B]) {
      def apply(from: A): B = self(from)

      def asFunction: A => B = self
    }

    inline def apply[A, B](fn: Transform.Generic[A, B] => A => B): Transform.Refined[A, B] = {
      given refined: Transform.Refined[A, B] = Refined.fromFunction(from => fn(generic)(from))
      lazy val generic = compiletime.summonInline[Transform.Generic[A, B]]
      refined
    }

    def fromFunction[A, B](fn: A => B): Transform.Refined[A, B] = fn

    inline def fromPartialFunction[A, B](fn: PartialFunction[A, B]): Transform.Refined[A, B] =
      Refined(generic => from => fn.applyOrElse(from, Generic.asFunction(generic)))
  }
}
