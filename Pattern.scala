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

  given monoidK(using DebugInfo): cats.MonoidK[Pattern] with
    export semigroupK.combineK
    def empty[A]: Pattern[A] =
      Pattern(Manip.monoidK.empty)

  protected transparent trait NodeTypeTest[T <: Node.All](using
      typeTest: TypeTest[Node.All, T]
  ) extends PartialFunction[Node.All, T]:
    def isDefinedAt(node: Node.All): Boolean =
      node match
        case typeTest(_) => true
        case _           => false
    def apply(node: Node.All): T =
      require(isDefinedAt(node))
      node.asInstanceOf[T]

  case object IsTop extends NodeTypeTest[Node.Top]
  case object IsParent extends NodeTypeTest[Node.Parent]
  case object IsChild extends NodeTypeTest[Node.Child]
  case object IsNode extends NodeTypeTest[Node]

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
      Pattern(Manip.Backtrack(summon[DebugInfo]))

    @scala.annotation.targetName("atHandlePattern")
    def atHandle[T](handle: Manip.Handle)(pattern: Pattern[T]): Pattern[T] =
      Pattern(Manip.ops.atHandle(handle)(pattern.manip))

    @scala.annotation.targetName("getHandlePattern")
    def getHandle(using DebugInfo): Pattern[Manip.Handle] =
      Pattern(Manip.ops.getHandle.map((-1, _)))

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

    final class Fields[Tpl <: Tuple] private (
        private val pattern: Pattern[(Tpl, Manip.Handle)]
    ):
      def field[T](using
          DebugInfo
      )(pattern: Pattern[T]): Fields[Tuple.Append[Tpl, T]] =
        new Fields(
          this.pattern.flatMap: (tpl, handle) =>
            atHandle(handle)(
              pattern.map(tpl :* _).product(rightSibling(getHandle))
            )
        )

      def skip[T](using DebugInfo)(pattern: Pattern[T]): Fields[Tpl] =
        new Fields(
          this.pattern.flatMap: (tpl, handle) =>
            atHandle(handle)(pattern *> rightSibling(getHandle)).map((tpl, _))
        )

      def optional[T](using
          DebugInfo
      )(pattern: Pattern[T]): Fields[Tuple.Append[Tpl, Option[T]]] =
        new Fields(
          this.pattern.flatMap: (tpl, handle) =>
            atHandle(handle):
              pattern.map(v => tpl :* Some(v)).product(rightSibling(getHandle))
                | pure((tpl :* None, handle))
        )

      def atEnd(using DebugInfo): Pattern[Tpl] =
        pattern.flatMap: (tpl, handle) =>
          atHandle(handle)(atEnd) *> pure(tpl)

      def trailing: Pattern[Tpl] =
        pattern.map(_._1)

      // TODO: repeat

    object Fields:
      def apply(): Fields[EmptyTuple] = new Fields(
        getHandle.map((EmptyTuple, _))
      )

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

    extension [T](lhs: Pattern[T])
      @scala.annotation.targetName("orPattern")
      infix def |(rhs: Pattern[T]): Pattern[T] =
        Pattern(Manip.ops.`|`(lhs.manip)(rhs.manip))
      @scala.annotation.targetName("restrictPattern")
      def restrict[U](using DebugInfo)(fn: PartialFunction[T, U]): Pattern[U] =
        Pattern:
          Manip.ops.restrict(lhs.manip):
            case (count, fn(value)) => (count, value)
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
      def withChildren[T](pattern: Pattern[T]): Pattern[T] =
        here(children(pattern))
      def withParent[T](pattern: Pattern[T]): Pattern[T] =
        here(parent(pattern))

    extension (nodePat: Pattern[Node])
      def filterSrc(using DebugInfo)(str: String): Pattern[Node] =
        nodePat.filterSrc(SourceRange.entire(Source.fromString(str)))
      def filterSrc(using DebugInfo)(src: SourceRange): Pattern[Node] =
        nodePat.filter(_.sourceRange == src)

    private def current: Pattern[Manip.Handle] =
      Pattern:
        import Manip.Handle
        Manip.ops.getHandle.map: handle =>
          handle.assertCoherence()
          handle match
            case Handle.AtTop(top)         => (-1, handle)
            case Handle.AtChild(_, idx, _) => (idx, handle)
            case Handle.Sentinel(_, idx)   => (idx, handle)

    def allNode(using DebugInfo): Pattern[Node.All] =
      import Manip.Handle
      current.flatMap:
        case Handle.AtTop(top)           => pure(top)
        case Handle.AtChild(_, _, child) => pure(child)
        case Handle.Sentinel(_, _)       => reject

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

    def embedValue[T: Tag]: Pattern[T] =
      embed[T].map(_.value)

    def not(using DebugInfo)(pattern: Pattern[?]): Pattern[Unit] =
      Pattern:
        Manip
          .Negated(pattern.manip, summon[DebugInfo])
          .map(_ => (0, ()))

    def tok(using DebugInfo)(token: Token, tokens: Token*): Pattern[Node] =
      allNode.restrict(NodeHasToken(token, tokens*))

    def oneOfToks(using DebugInfo)(tokens: Iterable[Token]): Pattern[Node] =
      val iter = tokens.iterator
      if !iter.hasNext
      then reject
      else
        val head = iter.next()
        allNode.restrict(NodeHasToken(head, iter.toSeq*))

    def parent[T](using DebugInfo)(pattern: Pattern[T]): Pattern[T] =
      anyChild
        .map(_.parent)
        .filter(_.nonEmpty)
        .map(_.get)
        .here(pattern)

    def ancestor[T](pattern: Pattern[T]): Pattern[T] =
      lazy val impl: Pattern[T] =
        pattern | defer(parent(impl))

      parent(impl)

    def repeated[T](pattern: Pattern[T]): Pattern[List[T]] =
      lazy val impl: Pattern[Chain[T]] =
        (pattern *: rightSibling(defer(impl)))
          .map(_ +: _)
          | Chain.empty.pure

      impl.map(_.toList)

    def rightSibling[T](pattern: Pattern[T]): Pattern[T] =
      Pattern(Manip.ops.atRightSibling(pattern.manip))

    def firstChild[T](pattern: Pattern[T]): Pattern[T] =
      Pattern(Manip.ops.atFirstChild(pattern.dropIdx.manip))

    def anyAtom(using DebugInfo): Pattern[Node] =
      anyNode.filter(_.children.isEmpty)

    // Convenience alias, because it looks better in context.
    // Lets us write children(...) when we mean all children,
    // even though technically we're just moving to the first child.
    def children[T](pattern: Pattern[T]): Pattern[T] =
      firstChild(pattern)

    @scala.annotation.targetName("deferPattern")
    def defer[T](pattern: => Pattern[T]): Pattern[T] =
      Pattern(Manip.defer(pattern.manip))

    def find[T](pattern: Pattern[T]): Pattern[T] =
      lazy val impl: Pattern[T] =
        pattern | defer(rightSibling(impl))

      impl

    def onlyChild[T](pattern: Pattern[T]): Pattern[T] =
      firstChild:
        pattern <* atEnd

    def refersToAny[T]: Pattern[List[Node]] =
      anyNode.map(_.lookup)

    def refersTo[T]: Pattern[Node] =
      refersToAny.restrict:
        case List(node) => node
  end ops
end Pattern
