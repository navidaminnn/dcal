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

  case object IsEnd extends NodeTypeTest[Node.RightSiblingSentinel]
  case object IsTop extends NodeTypeTest[Node.Top]
  case object IsParent extends NodeTypeTest[Node.Parent]
  case object IsSibling extends NodeTypeTest[Node.Sibling]
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

    @scala.annotation.targetName("logPattern")
    def log[T](using DebugInfo)(pattern: Pattern[T]): Pattern[T] =
      Pattern(Manip.ops.log(pattern.manip))

    def siblingAtOffset[T](using DebugInfo)(offset: Int)(
        pattern: Pattern[T]
    ): Pattern[T] =
      anySibling.flatMap: sibling =>
        val idx = sibling.idxInParent + offset
        val siblings = sibling.parent.children
        if siblings.isDefinedAt(idx) || siblings.isDefinedAt(idx - 1)
        then
          val target = siblings.findSibling(idx)
          Pattern(atNode(target)(Manip.pure((idx, ())) *> pattern.manip))
        else Pattern(backtrack)

    final class Fields[Tpl <: Tuple] private (
        val pattern: Pattern[Tpl],
        offset: Int
    ):
      def field[T](using
          DebugInfo
      )(pattern: Pattern[T]): Fields[Tuple.Append[Tpl, T]] =
        new Fields(
          (this.pattern, siblingAtOffset(offset)(pattern)).mapN(_ :* _),
          offset + 1
        )

      def skip[T](using DebugInfo)(pattern: Pattern[T]): Fields[Tpl] =
        new Fields(this.pattern <* siblingAtOffset(offset)(pattern), offset + 1)

      def atEnd(using DebugInfo): Pattern[Tpl] =
        skip(ops.atEnd).pattern

      // TODO: repeat

    object Fields:
      def apply(): Fields[EmptyTuple] = new Fields(pure(EmptyTuple), 0)

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

    private def current: Pattern[Node.All] =
      Pattern:
        Manip.ThisNode.map: node =>
          node match
            case sibling: Node.Sibling =>
              (sibling.idxInParent, node)
            case _: Node.Root =>
              (-1, node)

    def allNode: Pattern[Node.All] = current

    def atEnd(using DebugInfo): Pattern[Node.RightSiblingSentinel] =
      current.restrict(IsEnd)

    def anyNode(using DebugInfo): Pattern[Node] =
      current.restrict(IsNode)

    def anyParent(using DebugInfo): Pattern[Node.Parent] =
      current.restrict(IsParent)

    def anySibling(using DebugInfo): Pattern[Node.Sibling] =
      current.restrict(IsSibling)

    def theTop(using DebugInfo): Pattern[Node.Top] =
      current.restrict(IsTop)

    def anyChild(using DebugInfo): Pattern[Node.Child] =
      current.restrict(IsChild)

    def embed[T](using
        tag: Tag[T],
        debugInfo: DebugInfo
    ): Pattern[Node.Embed[T]] =
      current.restrict(IsEmbed(tag))

    def embedValue[T: Tag]: Pattern[T] =
      embed[T].map(_.value)

    def not(using DebugInfo)(pattern: Pattern[?]): Pattern[Unit] =
      Pattern:
        Manip
          .Negated(pattern.manip, summon[DebugInfo])
          .map(_ => (0, ()))

    def tok(using DebugInfo)(token: Token, tokens: Token*): Pattern[Node] =
      current.restrict(NodeHasToken(token, tokens*))

    def oneOfToks(using DebugInfo)(tokens: Iterable[Token]): Pattern[Node] =
      val iter = tokens.iterator
      if !iter.hasNext
      then reject
      else
        val head = iter.next()
        current.restrict(NodeHasToken(head, iter.toSeq*))

    def parent[T](pattern: Pattern[T]): Pattern[T] =
      anyChild.map(_.parent).here(pattern)

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
      anyChild
        .map(_.rightSibling)
        .flatMap: node =>
          Pattern(atNode(node)(pattern.manip))

    def firstChild[T](pattern: Pattern[T]): Pattern[T] =
      anyParent.map(_.firstChild).here(pattern)

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
        pattern <* rightSibling(atEnd)

    def refersToAny[T]: Pattern[List[Node]] =
      anyNode.map(_.lookup)

    def refersTo[T]: Pattern[Node] =
      refersToAny.restrict:
        case List(node) => node
  end ops
end Pattern
