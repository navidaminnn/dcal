// Copyright 2024-2025 Forja Team
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

package forja.manip

import cats.syntax.all.given
import forja.*
import forja.dsl.*
import scala.util.NotGiven

final class SeqPattern[+T](val manip: Manip[SeqPattern.Result[T]]):
  private inline given poison(using NotGiven[DebugInfo.Ctx]): DebugInfo =
    DebugInfo.poison
  import SeqPattern.*

  def |[U >: T](using DebugInfo.Ctx)(other: SeqPattern[U]): SeqPattern[U] =
    SeqPattern(manip.combineK(other.manip))

  def productAtRightSibling[U](using
      DebugInfo.Ctx,
  )(other: SeqPattern[U]): SeqPattern[(T, U)] =
    SeqPattern:
      manip.lookahead.flatMap:
        case Result.Top(_, _) => backtrack
        case Result.Look(_, idx, t) =>
          atIdx(idx)(other.map((t, _)).manip)
        case Result.Match(_, idx, t) =>
          atIdx(idx + 1)(other.map((t, _)).manip)

  def restrict[U](using
      DebugInfo.Ctx,
  )(fn: PartialFunction[T, U]): SeqPattern[U] =
    SeqPattern:
      manip.restrict:
        case Result.Top(top, fn(u))           => Result.Top(top, u)
        case Result.Look(parent, idx, fn(u))  => Result.Look(parent, idx, u)
        case Result.Match(parent, idx, fn(u)) => Result.Match(parent, idx, u)

  def filter(using DebugInfo.Ctx)(pred: T => Boolean): SeqPattern[T] =
    SeqPattern(manip.filter(res => pred(res.value)))

  def asManip: Manip[T] =
    manip.map(_.value)

  @scala.annotation.targetName("fieldsConcat")
  def ~[Tpl1 <: Tuple, Tpl2 <: Tuple](using
      T <:< Fields[Tpl1],
  )(using
      DebugInfo.Ctx,
  )(
      other: SeqPattern[Fields[Tpl2]],
  ): SeqPattern[Fields[Tuple.Concat[Tpl1, Tpl2]]] =
    this
      .productAtRightSibling(other)
      .map: (flds1, flds2) =>
        Fields(flds1.fields ++ flds2.fields)

  @scala.annotation.targetName("fieldsEnd")
  def ~[Tpl <: Tuple, T2](using
      T <:< Fields[Tpl],
  )(using
      maybeStrip: Fields.MaybeStripTuple1[Tpl, T2],
  )(using DebugInfo.Ctx)(other: eof.type): SeqPattern[T2] =
    this.productAtRightSibling(atEnd).map(flds => maybeStrip(flds._1.fields))

  @scala.annotation.targetName("fieldsTrailing")
  def ~[Tpl <: Tuple, T2](using
      T <:< Fields[Tpl],
  )(using
      maybeStrip: Fields.MaybeStripTuple1[Tpl, T2],
  )(
      other: trailing.type,
  ): SeqPattern[T2] =
    this.map(flds => maybeStrip(flds.fields))

  def stripFields[TT](using T <:< Fields[TT]): SeqPattern[TT] =
    this.map(t => t.fields)

  @scala.annotation.targetName("logicalAnd")
  def &&(other: SeqPattern[?]): SeqPattern[Unit] =
    this *> other.void

object SeqPattern:
  private inline given poison(using NotGiven[DebugInfo.Ctx]): DebugInfo =
    DebugInfo.poison
  export applicative.{pure, unit}

  given applicative: cats.Applicative[SeqPattern] with
    override val unit: SeqPattern[Unit] = pure(())
    def pure[A](x: A): SeqPattern[A] = SeqPattern:
      getHandle.map:
        case Handle.AtTop(top) =>
          Result.Top(top, x)
        case handle: (Handle.Sentinel | Handle.AtChild) =>
          Result.Look(handle.parent, handle.idx, x)
    override def map[A, B](fa: SeqPattern[A])(f: A => B): SeqPattern[B] =
      SeqPattern:
        fa.manip.map: result =>
          result.withValue(f(result.value))
    def ap[A, B](ff: SeqPattern[A => B])(fa: SeqPattern[A]): SeqPattern[B] =
      SeqPattern:
        (ff.manip, fa.manip).mapN: (result1, result2) =>
          result1.combine(result2)(_.apply(_))

  given semigroupk: cats.SemigroupK[SeqPattern] with
    def combineK[A](x: SeqPattern[A], y: SeqPattern[A]): SeqPattern[A] =
      SeqPattern(x.manip | y.manip)

  enum Result[+T]:
    val value: T

    case Top(top: Node.Top, value: T)
    case Look(parent: Node.Parent, idx: Int, value: T)
    case Match(parent: Node.Parent, idx: Int, value: T)

    def isLook: Boolean =
      this match
        case _: Look[?] => true
        case _          => false

    def isMatch: Boolean =
      this match
        case _: Match[?] => true
        case _           => false

    def withValue[U](value: U): Result[U] =
      if this.value.asInstanceOf[AnyRef] eq value.asInstanceOf[AnyRef]
      then this.asInstanceOf
      else
        this match
          case Top(top, _)           => Top(top, value)
          case Look(parent, idx, _)  => Look(parent, idx, value)
          case Match(parent, idx, _) => Match(parent, idx, value)

    def combine[U, V](other: Result[U])(fn: (T, U) => V): Result[V] =
      def result = fn(value, other.value)
      (this, other) match
        case (Top(top1, _), Top(top2, _)) =>
          assert(top1 eq top2)
          Top(top1, result)
        case (Top(_, _), _) | (_, Top(_, _)) =>
          throw NodeError(
            "one part of the same pattern matched top while the other did not",
          )
        case (lhs: (Look[T] | Match[T]), rhs: (Look[U] | Match[U])) =>
          assert(rhs.parent eq rhs.parent)
          val parent = lhs.parent
          def mkLook(idx: Int): Result[V] =
            Look(parent, idx, result)
          def mkMatch(idx: Int): Result[V] =
            Match(parent, idx, result)

          (lhs, rhs) match
            case (Look(_, idx1, _), Look(_, idx2, _)) =>
              mkLook(idx1.max(idx2))
            case (Look(_, idx1, _), Match(_, idx2, _)) if idx1 <= idx2 =>
              mkMatch(idx2)
            case (Look(_, idx1, _), Match(_, idx2, _)) =>
              assert(idx1 > idx2)
              mkLook(idx1)
            case (Match(_, idx1, _), Look(_, idx2, _)) if idx1 < idx2 =>
              mkLook(idx2)
            case (Match(_, idx1, _), Look(_, idx2, _)) =>
              assert(idx1 >= idx2)
              mkMatch(idx1)
            case (Match(_, idx1, _), Match(_, idx2, _)) =>
              mkMatch(idx1.max(idx2))

  object Result:
    extension [T](result: Result.Look[T] | Result.Match[T])
      def parent: Node.Parent =
        result match
          case Result.Look(parent, _, _)  => parent
          case Result.Match(parent, _, _) => parent
      def idx: Int =
        result match
          case Result.Look(_, idx, _)  => idx
          case Result.Match(_, idx, _) => idx

  import scala.language.implicitConversions
  implicit def tokenAsTok(using DebugInfo)(token: Token): SeqPattern[Node] =
    tok(token)

  final class Fields[T](val fields: T) extends AnyVal
  object Fields:
    sealed trait MaybeStripTuple1[Tpl <: Tuple, T]:
      def apply(tpl: Tpl): T

    given stripTuple1[T]: MaybeStripTuple1[Tuple1[T], T] with
      def apply(tpl: Tuple1[T]): T = tpl._1

    given notTuple1[Tpl <: Tuple](using
        scala.util.NotGiven[Tpl <:< Tuple1[?]],
    ): MaybeStripTuple1[Tpl, Tpl] with
      def apply(tpl: Tpl): Tpl = tpl

  case object FieldsEndMarker
  case object FieldsTrailingMarker
