package com.github.distcompiler.dcal.transform

import cats.*
import cats.syntax.all.given
import com.github.distcompiler.dcal.util.{SummonFallback, SummonTuple}
import scala.annotation.tailrec
import scala.annotation.targetName

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

  def genericFromFunction[A, B](fn: A => B): Transform.Generic[A, B] = fn

  extension [A, B](self: Transform[A, B]) {
    def asFunction: A => B = self

    def apply(from: A): B = (self: A => B).apply(from)

    // def asGeneric: Transform.Generic[A, B] = self

    // def asRefined: Transform.Refined[A, B] = self
  }

  //def fromGeneric[A, B](generic: Generic[A, B]): Transform[A, B] = generic

  given refinedOrGeneric[A, B](using choice: SummonFallback[Refined[A, B], Generic[A, B]]): Transform[A, B] =
    choice.value match {
      case Left(generic) => generic
      case Right(refined) => refined
    }

  opaque type Generic[A, B] = Transform[A, B]

  extension [A, B](self: Generic[A, B]) {
    @targetName("applyGeneric")
    def apply(from: A): B = self(from)
  }

  opaque type Refined[A, B] = Transform[A, B]

  extension [A, B](self: Refined[A, B]) {
    @targetName("applyRefined")
    def apply(from: A): B = self(from)

    @targetName("refinedAsFunction")
    def asFunction: A => B = self
  }

  object Refined {
    inline def apply[A, B](fn: Transform.Generic[A, B] => A => B): Transform.Refined[A, B] = {
      // see below for cast rationale
      given refined: Transform.Refined[A, B] = (from => fn(generic)(from)).asInstanceOf[Transform.Refined[A, B]]
      lazy val generic = compiletime.summonInline[Transform.Generic[A, B]]
      refined
    }

    inline def fromPartialFunction[A, B](fn: PartialFunction[A, B]): Transform.Refined[A, B] =
      // that type cast exists because in an expanded macro we don't get the benefit of being in this file (can't see through opaque type)
      // also, because we are in this file now, the asFunction extension appears ambiguous even though it won't be at expansion time, so we can't use it
      Refined(generic => from => fn.applyOrElse(from, generic.asInstanceOf[A => B]))
  }
}
