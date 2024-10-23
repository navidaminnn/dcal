package distcompiler

import cats.syntax.all.given
import dsl.*

final class SeqPattern[+T](val manip: Manip[(Manip.Handle, Boolean, T)])
    extends AnyVal:
  import SeqPattern.*

  def |[U >: T](other: SeqPattern[U]): SeqPattern[U] =
    SeqPattern(manip.combineK(other.manip))

  def productAtRightSibling[U](other: SeqPattern[U]): SeqPattern[(T, U)] =
    SeqPattern:
      manip.lookahead.flatMap: (handle, matched, t) =>
        atHandle(handle):
          if matched
          then
            atRightSibling:
              other.map((t, _)).manip
          else other.map((t, _)).manip

  def restrict[U](fn: PartialFunction[T, U]): SeqPattern[U] =
    SeqPattern:
      manip.restrict:
        case (handle, matched, fn(u)) => (handle, matched, u)

  def filter(pred: T => Boolean): SeqPattern[T] =
    SeqPattern(manip.filter(p => pred(p._3)))

  def asManip: Manip[T] =
    manip.map(_._3)

  @scala.annotation.targetName("fieldsConcat")
  def ~[Tpl1 <: Tuple, Tpl2 <: Tuple](using
      T <:< Fields[Tpl1]
  )(using DebugInfo)(
      other: SeqPattern[Fields[Tpl2]]
  ): SeqPattern[Fields[Tuple.Concat[Tpl1, Tpl2]]] =
    this
      .productAtRightSibling(other)
      .map: (flds1, flds2) =>
        Fields(flds1.fields ++ flds2.fields)

  @scala.annotation.targetName("fieldsEnd")
  def ~[Tpl <: Tuple](using T <:< Fields[Tpl])(using
      scala.util.NotGiven[Tpl <:< Tuple1[?]]
  )(using DebugInfo)(other: eof.type): SeqPattern[Tpl] =
    this.productAtRightSibling(atEnd).map(_._1.fields)

  @scala.annotation.targetName("fieldsTrailing")
  def ~[Tpl <: Tuple](using
      T <:< Fields[Tpl]
  )(using scala.util.NotGiven[Tpl <:< Tuple1[?]])(
      other: trailing.type
  ): SeqPattern[Tpl] =
    this.map(_.fields)

  @scala.annotation.targetName("fieldsEndTuple1")
  def ~[TT](using T <:< Fields[Tuple1[TT]])(using DebugInfo)(
      other: eof1.type
  ): SeqPattern[TT] =
    this.productAtRightSibling(atEnd).map(_._1.fields._1)

  @scala.annotation.targetName("fieldsTrailingTuple1")
  def ~[TT](using T <:< Fields[Tuple1[TT]])(
      other: trailing1.type
  ): SeqPattern[TT] =
    this.map(_.fields._1)

  def stripFields[TT](using T <:< Fields[TT]): SeqPattern[TT] =
    this.map(t => t.fields)

object SeqPattern:
  export applicative.{pure, unit}

  given applicative: cats.Applicative[SeqPattern] with
    override val unit: SeqPattern[Unit] = pure(())
    def pure[A](x: A): SeqPattern[A] = SeqPattern(getHandle.map((_, false, x)))
    override def map[A, B](fa: SeqPattern[A])(f: A => B): SeqPattern[B] =
      SeqPattern:
        fa.manip.map: pair =>
          val (handle, matched, a) = pair
          val b = f(a)
          // optimization: if it's literally the same object, don't remake the tuple
          if a.asInstanceOf[AnyRef] eq b.asInstanceOf[AnyRef]
          then pair.asInstanceOf[(Manip.Handle, Boolean, B)]
          else (handle, matched, b)
    def ap[A, B](ff: SeqPattern[A => B])(fa: SeqPattern[A]): SeqPattern[B] =
      SeqPattern:
        (ff.manip, fa.manip).mapN:
          case ((handle1, matched1, ff), (handle2, matched2, fa)) =>
            val matched = matched1 || matched2
            val value = ff(fa)
            if handle1 eq handle2
            then (handle1, matched, value)
            else
              import Manip.Handle
              (handle1, handle2) match
                case (Handle.AtTop(top1), Handle.AtTop(top2)) =>
                  assert(top1 eq top2)
                  (handle1, matched, value)
                case (
                      Handle.AtChild(parent1, idx1, child1),
                      Handle.AtChild(parent2, idx2, child2)
                    ) =>
                  assert(parent1 eq parent2)
                  if idx1 < idx2
                  then (handle2, matched, value)
                  else (handle1, matched, value)
                case (
                      Handle.AtChild(parent1, idx1, _),
                      Handle.Sentinel(parent2, idx2)
                    ) =>
                  assert(parent1 eq parent2)
                  assert(idx1 < idx2)
                  (handle2, matched, value)
                case (
                      Handle.Sentinel(parent1, idx1),
                      Handle.AtChild(parent2, idx2, _)
                    ) =>
                  assert(parent1 eq parent2)
                  assert(idx1 > idx2)
                  (handle1, matched, value)
                case (
                      Handle.Sentinel(parent1, idx1),
                      Handle.Sentinel(parent2, idx2)
                    ) =>
                  assert(parent1 eq parent2)
                  assert(idx1 == idx2)
                  (handle1, matched, value)
                case _ =>
                  assert(false); ???

  given semigroupk: cats.SemigroupK[SeqPattern] with
    def combineK[A](x: SeqPattern[A], y: SeqPattern[A]): SeqPattern[A] =
      SeqPattern(x.manip | y.manip)

  final case class NodeTokensRestriction(tokens: Set[Token])
      extends Manip.Restriction[Node.All, Node]:
    protected val impl = {
      case node: Node if tokens(node.token) => node
    }

  import scala.language.implicitConversions
  implicit def tokenAsTok(token: Token): SeqPattern[Node] =
    tok(token)

  final class Fields[T](val fields: T) extends AnyVal
  case object FieldsEndMarker
  case object FieldsTrailingMarker
  case object FieldsEndTuple1Marker
  case object FieldsTrailingTuple1Marker

  object ops:
    def refine[T](manip: Manip[T]): SeqPattern[T] =
      SeqPattern:
        (getHandle, Manip.pure(false), manip).tupled

    @scala.annotation.targetName("deferSeqPattern")
    def defer[T](pattern: => SeqPattern[T]): SeqPattern[T] =
      SeqPattern(Manip.ops.defer(pattern.manip))

    def field[T](pattern: SeqPattern[T]): SeqPattern[Fields[Tuple1[T]]] =
      pattern.map(t => Fields(Tuple1(t)))

    def fields[Tpl <: Tuple](
        pattern: SeqPattern[Tpl]
    ): SeqPattern[Fields[Tpl]] =
      pattern.map(Fields(_))

    def skip[T](pattern: SeqPattern[T]): SeqPattern[Fields[EmptyTuple]] =
      pattern.as(Fields(EmptyTuple))

    def anyNode(using DebugInfo): SeqPattern[Node] =
      SeqPattern:
        (
          getHandle,
          Manip.pure(true),
          getNode.restrict { case node: Node => node }
        ).tupled

    def anyChild(using DebugInfo): SeqPattern[Node.Child] =
      SeqPattern:
        (
          getHandle,
          Manip.pure(true),
          getNode.restrict { case child: Node.Child => child }
        ).tupled

    def tok(using DebugInfo)(tokens: Token*): SeqPattern[Node] =
      SeqPattern:
        (
          getHandle,
          Manip.pure(true),
          getNode.restrict(NodeTokensRestriction(tokens.toSet))
        ).tupled

    def atEnd(using DebugInfo): SeqPattern[Unit] =
      SeqPattern:
        getHandle.restrict:
          case handle @ Manip.Handle.Sentinel(_, _) => (handle, false, ())

    export SeqPattern.{
      FieldsEndMarker as eof,
      FieldsEndTuple1Marker as eof1,
      FieldsTrailingMarker as trailing,
      FieldsTrailingTuple1Marker as trailing1
    }

    def not[T](using DebugInfo)(pattern: SeqPattern[T]): SeqPattern[Unit] =
      SeqPattern(
        Manip.Negated(pattern.manip, summon[DebugInfo]) *> getHandle.map(
          (_, false, ())
        )
      )

    def optional[T](using DebugInfo)(
        pattern: SeqPattern[T]
    ): SeqPattern[Option[T]] =
      pattern.map(Some(_)) | pure(None)

    def repeated[T](using DebugInfo)(
        pattern: SeqPattern[T]
    ): SeqPattern[List[T]] =
      lazy val impl: SeqPattern[List[T]] =
        (field(pattern) ~ field(defer(impl)) ~ trailing)
          .map(_ :: _)
          | pure(Nil)

      impl

    def repeatedSepBy1[T](using
        DebugInfo
    )(sep: SeqPattern[?])(pattern: SeqPattern[T]): SeqPattern[List[T]] =
      (field(pattern) ~ field(
        repeated(skip(sep) ~ field(pattern) ~ trailing1)
      ) ~ trailing)
        .map(_ :: _)

    def repeatedSepBy[T](using DebugInfo)(sep: SeqPattern[?])(
        pattern: SeqPattern[T]
    ): SeqPattern[List[T]] =
      repeatedSepBy1(sep)(pattern)
        | pure(Nil)

    def theTop(using DebugInfo): SeqPattern[Node.Top] =
      SeqPattern:
        getHandle.restrict:
          case handle @ Manip.Handle.AtTop(top) => (handle, true, top)

    def children[T](using DebugInfo)(pattern: SeqPattern[T]): SeqPattern[T] =
      refine(atFirstChild(pattern.asManip))

    def parent[T](using DebugInfo)(pattern: SeqPattern[T]): SeqPattern[T] =
      refine(atParent(on(pattern).value))

    def ancestor[T](using DebugInfo)(pattern: SeqPattern[T]): SeqPattern[T] =
      refine(atAncestor(on(pattern).value))

    def lastChild[T](using DebugInfo)(pattern: SeqPattern[T]): SeqPattern[T] =
      refine(atLastChild(on(pattern).value))

    extension [P <: Node.Parent](parentPattern: SeqPattern[P])
      def withChildren[T](using DebugInfo)(
          pattern: SeqPattern[T]
      ): SeqPattern[T] =
        SeqPattern:
          parentPattern.manip.lookahead.flatMap: (handle, matched, parent) =>
            atNode(parent)(atFirstChild(pattern.asManip))
              .map((handle, matched, _))

    extension (nodePattern: SeqPattern[Node])
      def src(using DebugInfo)(sourceRange: SourceRange): SeqPattern[Node] =
        nodePattern.filter(_.sourceRange == sourceRange)

      def src(using DebugInfo)(str: String): SeqPattern[Node] =
        src(SourceRange.entire(Source.fromString(str)))

    extension [T](hdPattern: SeqPattern[T])
      def *:[Tpl <: Tuple](tlPattern: SeqPattern[Tpl]): SeqPattern[T *: Tpl] =
        (hdPattern, tlPattern).mapN(_ *: _)
