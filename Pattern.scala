package distcompiler

import cats.data.Chain
import cats.syntax.all.given
import scala.util.NotGiven
import izumi.reflect.Tag
import scala.reflect.TypeTest

final class Pattern[+T](val manip: Manip[(Int, T)]) extends AnyVal:
  def isBacktrack: Boolean = manip.isBacktrack

object Pattern:
  import Manip.ops.{*, given}
  private inline given DebugInfo = DebugInfo.poison

  export applicative.{pure, unit}
  export ops.reject

  given applicative: cats.Applicative[Pattern] with
    override val unit: Pattern[Unit] = pure(())
    def pure[A](value: A): Pattern[A] =
      Pattern(Manip.pure((0, value)))
    def ap[A, B](ff: Pattern[A => B])(fa: Pattern[A]): Pattern[B] =
      Pattern:
        (ff.manip, fa.manip).mapN:
          case ((progL, ff), (progR, fa)) =>
            (progL.max(progR), ff(fa))

  given semigroupK: cats.SemigroupK[Pattern] with
    def combineK[A](x: Pattern[A], y: Pattern[A]): Pattern[A] =
      Pattern(x.manip.combineK(y.manip))

  // given monoidK(using DebugInfo): cats.MonoidK[Pattern] with
  //   export semigroupK.combineK
  //   def empty[A]: Pattern[A] =
  //     Pattern(Manip.monoidK.empty)

  private transparent trait NodeTypeTest[T <: Node.All](using
      typeTest: TypeTest[Node.All, T]
  ) extends Manip.Restriction[Node.All, T]:
    protected val impl = { case typeTest(value) =>
      value
    }

  private case object IsTop extends NodeTypeTest[Node.Top]
  private case object IsParent extends NodeTypeTest[Node.Parent]
  private case object IsChild extends NodeTypeTest[Node.Child]
  private case object IsNode extends NodeTypeTest[Node]

  final case class NodeHasToken(token: Token, tokens: Token*)
      extends PartialFunction[Node.All, Node]:
    def isDefinedAt(node: Node.All): Boolean =
      node match
        case node: Node if node.token == token || tokens.contains(node.token) =>
          true
        case _ => false
    def apply(node: Node.All): Node =
      require(isDefinedAt(node))
      node.asInstanceOf[Node]

  final case class IsEmbed[T](tag: Tag[T])
      extends PartialFunction[Node.All, Node.Embed[T]]:
    def isDefinedAt(node: Node.All): Boolean =
      node match
        case embed: Node.Embed[?] if embed.nodeMeta.tag <:< tag => true
        case _                                                  => false
    def apply(node: Node.All): Node.Embed[T] =
      require(isDefinedAt(node))
      node.asInstanceOf[Node.Embed[T]]

  object ops:
    def reject(using DebugInfo): Pattern[Nothing] =
      Pattern(Manip.ops.backtrack)

    @scala.annotation.targetName("atHandlePattern")
    def atHandle[T](handle: Manip.Handle)(pattern: Pattern[T]): Pattern[T] =
      Pattern(Manip.ops.atHandle(handle)(pattern.manip))

    private case object RestrictGetHandle
        extends Manip.Restriction[Manip.Handle, (Int, Manip.Handle)]:
      protected val impl = (-1, _)

    @scala.annotation.targetName("withTracerPattern")
    def withTracer[T](tracer: => Manip.Tracer)(
        pattern: Pattern[T]
    ): Pattern[T] =
      Pattern(Manip.ops.withTracer(pattern.manip)(tracer))

    @scala.annotation.targetName("getHandlePattern")
    def getHandle(using DebugInfo): Pattern[Manip.Handle] =
      Pattern(Manip.ops.restrict(Manip.ops.getHandle)(RestrictGetHandle))

    def siblingAtOffset[T](using DebugInfo)(offset: Int)(
        pattern: Pattern[T]
    ): Pattern[T] =
      anyChild
        .filter(_.parent.nonEmpty)
        .flatMap: sibling =>
          val idx = sibling.idxInParent + offset
          val siblings = sibling.parent.get.children
          if siblings.isDefinedAt(idx)
          then
            val target = siblings(idx)
            Pattern(atNode(target)(Manip.pure((idx, ())) *> pattern.manip))
          else reject

    abstract class Fields[Tpl <: Tuple] private ():
      self =>
      protected inline given DebugInfo =
        DebugInfo.poison // block givens from outside local class defns
      import Tuple.{Concat, Append}
      import Fields.tupleAppendInitIdentity

      protected def withTail[Tl](info: DebugInfo)(
          tail: Pattern[Tl]
      ): Pattern[(Tpl, Tl)]

      final def field[T](using outerInfo: DebugInfo)(
          pattern: Pattern[T]
      ): Fields[Append[Tpl, T]] =
        new Fields[Append[Tpl, T]]:
          protected def withTail[Tl](info: DebugInfo)(
              tail: Pattern[Tl]
          ): Pattern[(Append[Tpl, T], Tl)] =
            self
              .withTail(outerInfo):
                pattern.product(rightSibling(using info)(tail))
              .map:
                case (tpl, (t, tl)) => (tpl :* t, tl)

      final def nestedFields[InnerTpl <: Tuple](
          innerFields: Fields[InnerTpl]
      ): Fields[Concat[Tpl, InnerTpl]] =
        new Fields[Concat[Tpl, InnerTpl]]:
          protected def withTail[Tl](
              info: DebugInfo
          )(tail: Pattern[Tl]): Pattern[(Concat[Tpl, InnerTpl], Tl)] =
            self
              .withTail(info)(innerFields.withTail(info)(tail))
              .map:
                case (tpl, (innerTpl, tl)) => (tpl ++ innerTpl, tl)

      final def map[Tpl2 <: Tuple](fn: Tpl => Tpl2): Fields[Tpl2] =
        new Fields[Tpl2]:
          protected def withTail[Tl](
              info: DebugInfo
          )(tail: Pattern[Tl]): Pattern[(Tpl2, Tl)] =
            self
              .withTail(info)(tail)
              .map((tpl, tl) => (fn(tpl), tl))

      final def dropLast(using
          ev: Tpl <:< NonEmptyTuple
      ): Fields[Tuple.Init[Tpl]] =
        map(tpl => ev(tpl).init.asInstanceOf)

      final def skip[T](using DebugInfo)(
          pattern: Pattern[T]
      ): Fields[Tpl] =
        field(pattern).dropLast.map(_.tupleAppendInitIdentity)

      final def optionalSkip[T](using outerInfo: DebugInfo)(
          pattern: Pattern[T]
      ): Fields[Tpl] =
        optional(pattern).dropLast.map(_.tupleAppendInitIdentity)

      final def optional[T](using DebugInfo)(
          pattern: Pattern[T]
      ): Fields[Append[Tpl, Option[T]]] =
        self
          .nestedFields:
            Fields().field(pattern.map(Some(_)))
              | Fields().map(_ => Tuple1(None))
          .map(_.asInstanceOf)

      final def optionalFields[InnerTpl <: Tuple](using
          outerInfo: DebugInfo
      )(innerFields: Fields[InnerTpl])(using
          innerFieldsSize: ValueOf[Tuple.Size[InnerTpl]]
      ): Fields[Append[Tpl, Option[InnerTpl]]] =
        self
          .nestedFields:
            innerFields.map(tpl => Tuple1(Some(tpl)))
              | Fields().map(_ => Tuple1(None))
          .map(_.asInstanceOf)

      final def repeated[T](using outerInfo: DebugInfo)(
          pattern: Pattern[T]
      ): Fields[Append[Tpl, List[T]]] =
        self
          .repeatedFields:
            Fields().field(pattern)
          .map: tpl =>
            val init = tpl.init.asInstanceOf[Tpl]
            val last = tpl.last.asInstanceOf[List[Tuple1[T]]]
            (init :* last.map(_._1))

      final def repeatedFields[InnerTpl <: Tuple](using outerInfo: DebugInfo)(
          fields: Fields[InnerTpl]
      ): Fields[Append[Tpl, List[InnerTpl]]] =
        new Fields[Append[Tpl, List[InnerTpl]]]:
          protected def withTail[Tl](info: DebugInfo)(
              tail: Pattern[Tl]
          ): Pattern[(Append[Tpl, List[InnerTpl]], Tl)] =
            self
              .withTail(outerInfo):
                lazy val impl: Pattern[(List[InnerTpl], Tl)] =
                  fields
                    .withTail(info)(defer(impl))
                    .map:
                      case (innerTplHd, (innerTplTl, tl)) =>
                        (innerTplHd :: innerTplTl, tl)
                  | pure(Nil).product(tail)

                impl
              .map:
                case (tpl, (innerTpls, tl)) =>
                  (tpl :* innerTpls, tl)

      final def repeatedSkip[T](using outerInfo: DebugInfo)(
          pattern: Pattern[T]
      ): Fields[Tpl] =
        repeated(pattern).dropLast.map(_.tupleAppendInitIdentity)

      final def |[UpTpl >: Tpl <: Tuple](
          otherFields: Fields[UpTpl]
      ): Fields[UpTpl] =
        new Fields[UpTpl]:
          protected def withTail[Tl](info: DebugInfo)(
              tail: Pattern[Tl]
          ): Pattern[(UpTpl, Tl)] =
            self.withTail(info)(tail)
              | otherFields.withTail(info)(tail)

      final def atEnd(using outerInfo: DebugInfo): Pattern[Tpl] =
        self
          .withTail(outerInfo):
            ops.atEnd.as(EmptyTuple)
          .map(_._1)

      final def trailing(using outerInfo: DebugInfo): Pattern[Tpl] =
        self
          .withTail(outerInfo)(pure(EmptyTuple))
          .map(_._1)

    object Fields:
      import Tuple.{Concat, Append}
      def apply(): Fields[EmptyTuple] = new Fields[EmptyTuple]:
        protected def withTail[Tl](info: DebugInfo)(
            tail: Pattern[Tl]
        ): Pattern[(EmptyTuple.type, Tl)] =
          tail.map((EmptyTuple, _))

      // Scala is not a theorem prover, but I can at least give the casts the right name
      extension [Tpl <: Tuple, T](before: Tuple.Init[Tuple.Append[Tpl, T]])
        private inline def tupleAppendInitIdentity: Tpl =
          before.asInstanceOf

    extension [T](lhs: Pattern[T])
      @scala.annotation.targetName("tupleCons")
      infix def *:[U <: Tuple](rhs: Pattern[U]): Pattern[T *: U] =
        (lhs, rhs).mapN(_ *: _)
      @scala.annotation.targetName("tupleConsPairOf")
      infix def *:[U](rhs: Pattern[U])(using
          NotGiven[U <:< Tuple]
      ): Pattern[(T, U)] =
        lhs.product(rhs)

    extension [T <: Tuple](lhs: Pattern[T])
      @scala.annotation.targetName("tupleAppend")
      infix def :*[U](rhs: Pattern[U]): Pattern[Tuple.Append[T, U]] =
        (lhs, rhs).mapN(_ :* _)

    extension [T](lhs: Pattern[T])(using NotGiven[T <:< Tuple])
      @scala.annotation.targetName("tupleAppendPairOf")
      infix def :*[U](rhs: Pattern[U]): Pattern[(T, U)] =
        lhs.product(rhs)

    extension [T <: Tuple](lhs: Pattern[T])
      @scala.annotation.targetName("tupleConcat")
      infix def ++:[U <: Tuple](rhs: Pattern[U]): Pattern[Tuple.Concat[T, U]] =
        (lhs, rhs).mapN(_ ++ _)

    private case class PatternRestriction[T, U](restr: Manip.Restriction[T, U])
        extends Manip.Restriction[(Int, T), (Int, U)]:
      protected val impl = { case (count, restr(value)) =>
        (count, value)
      }

    extension [T](lhs: Pattern[T])
      @scala.annotation.targetName("orPattern")
      infix def |(rhs: Pattern[T]): Pattern[T] =
        Pattern(Manip.ops.`|`(lhs.manip)(rhs.manip))
      @scala.annotation.targetName("restrictPattern")
      def restrict[U](using DebugInfo)(fn: PartialFunction[T, U]): Pattern[U] =
        Pattern:
          Manip.ops.restrict(lhs.manip)(
            PatternRestriction(Manip.Restriction(fn))
          )
      @scala.annotation.targetName("filterPattern")
      def filter(using DebugInfo)(fn: T => Boolean): Pattern[T] =
        lhs.restrict:
          case t if fn(t) => t
      def flatMap[U](fn: T => Pattern[U]): Pattern[U] =
        Pattern:
          lhs.manip.lookahead.flatMap: (count, value) =>
            fn(value).manip.map: (count2, value) =>
              (count.max(count2), value)
      private def dropIdx: Pattern[T] =
        Pattern(lhs.manip.map((_, value) => (-1, value)))

    extension (dest: Pattern[Node.All])
      @scala.annotation.targetName("patternHere")
      def here[T](pattern: Pattern[T]): Pattern[T] =
        dest.flatMap: node =>
          Pattern(atNode(node)(pattern.dropIdx.manip))
      def withChildren[T](using DebugInfo)(pattern: Pattern[T]): Pattern[T] =
        here(children(pattern))
      def withParent[T](using DebugInfo)(pattern: Pattern[T]): Pattern[T] =
        here(parent(pattern))

    extension (nodePat: Pattern[Node])
      def filterSrc(using DebugInfo)(str: String): Pattern[Node] =
        nodePat.filterSrc(SourceRange.entire(Source.fromString(str)))
      def filterSrc(using DebugInfo)(src: SourceRange): Pattern[Node] =
        nodePat.filter(_.sourceRange == src)

    private def current(using DebugInfo): Pattern[Manip.Handle] =
      Pattern:
        import Manip.Handle
        Manip.ops.getHandle.map: handle =>
          handle.assertCoherence()
          handle match
            case Handle.AtTop(top)         => (-1, handle)
            case Handle.AtChild(_, idx, _) => (idx, handle)
            case Handle.Sentinel(_, idx)   => (idx, handle)

    private case object RestrictAllNode
        extends Manip.Restriction[Manip.Handle, Node.All]:
      import Manip.Handle
      protected val impl = {
        case Handle.AtTop(top)           => top
        case Handle.AtChild(_, _, child) => child
      }

    def allNode(using DebugInfo): Pattern[Node.All] =
      current.restrict(RestrictAllNode)

    def atEnd(using DebugInfo): Pattern[Unit] =
      import Manip.Handle
      current.restrict:
        case Handle.Sentinel(_, _) => ()

    def anyNode(using DebugInfo): Pattern[Node] =
      allNode.restrict(IsNode)

    def anyParent(using DebugInfo): Pattern[Node.Parent] =
      allNode.restrict(IsParent)

    def theTop(using DebugInfo): Pattern[Node.Top] =
      allNode.restrict(IsTop)

    def anyChild(using DebugInfo): Pattern[Node.Child] =
      allNode.restrict(IsChild)

    def embed[T](using
        tag: Tag[T],
        debugInfo: DebugInfo
    ): Pattern[Node.Embed[T]] =
      allNode.restrict(IsEmbed(tag))

    def embedValue[T: Tag](using DebugInfo): Pattern[T] =
      embed[T].map(_.value)

    def not(using DebugInfo)(pattern: Pattern[?]): Pattern[Unit] =
      Pattern:
        Manip
          .Negated(pattern.manip, summon[DebugInfo])
          .map(_ => (0, ()))

    private case object GetNodeToken extends Manip.Restriction[Node.All, Token]:
      protected val impl = { case node: Node =>
        node.token
      }

    def tok(using DebugInfo)(token: Token, tokens: Token*): Pattern[Node] =
      Pattern:
        import Manip.ops.mkTable
        val table = Manip.ops
          .restrict(Manip.ops.getNode)(GetNodeToken)
          .mkTable(tokens.toSet.incl(token))
        Manip.ops.flatMap(table)(_ => anyNode.manip)

    import scala.language.implicitConversions
    // TODO: add a Conversion for this once we can use into?
    implicit def tokFromToken(token: Token)(using DebugInfo): Pattern[Node] =
      tok(token)

    def oneOfToks(using DebugInfo)(tokens: Iterable[Token]): Pattern[Node] =
      val iter = tokens.iterator
      if !iter.hasNext
      then reject
      else
        val head = iter.next()
        tok(head, iter.toSeq*)

    def parent[T](using DebugInfo)(pattern: Pattern[T]): Pattern[T] =
      anyChild
        .map(_.parent)
        .filter(_.nonEmpty)
        .map(_.get)
        .here(pattern)

    def ancestor[T](using DebugInfo)(pattern: Pattern[T]): Pattern[T] =
      lazy val impl: Pattern[T] =
        pattern | defer(parent(impl))

      parent(impl)

    def repeated[T](using DebugInfo)(pattern: Pattern[T]): Pattern[List[T]] =
      lazy val impl: Pattern[Chain[T]] =
        (pattern *: rightSibling(defer(impl)))
          .map(_ +: _)
          | Chain.empty.pure

      impl.map(_.toList)

    def rightSibling[T](using DebugInfo)(pattern: Pattern[T]): Pattern[T] =
      Pattern(Manip.ops.atRightSibling(pattern.manip))

    def firstChild[T](using DebugInfo)(pattern: Pattern[T]): Pattern[T] =
      Pattern(Manip.ops.atFirstChild(pattern.dropIdx.manip))

    def lastChild[T](using DebugInfo)(pattern: Pattern[T]): Pattern[T] =
      Pattern(Manip.ops.atLastChild(pattern.dropIdx.manip))

    def anyAtom(using DebugInfo): Pattern[Node] =
      anyNode.filter(_.children.isEmpty)

    // Convenience alias, because it looks better in context.
    // Lets us write children(...) when we mean all children,
    // even though technically we're just moving to the first child.
    def children[T](using DebugInfo)(pattern: Pattern[T]): Pattern[T] =
      firstChild(pattern)

    @scala.annotation.targetName("deferPattern")
    def defer[T](pattern: => Pattern[T]): Pattern[T] =
      Pattern(Manip.defer(pattern.manip))

    def find[T](using DebugInfo)(pattern: Pattern[T]): Pattern[T] =
      lazy val impl: Pattern[T] =
        pattern | defer(rightSibling(impl))

      impl

    def onlyChild[T](using DebugInfo)(pattern: Pattern[T]): Pattern[T] =
      firstChild:
        pattern <* rightSibling(atEnd)

    def refersToAny[T](using DebugInfo): Pattern[List[Node]] =
      anyNode.map(_.lookup)

    def refersTo[T](using DebugInfo): Pattern[Node] =
      refersToAny.restrict:
        case List(node) => node
  end ops
end Pattern
