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

    extension [T](lhs: Pattern[T])
      infix def *>:[U](rhs: Pattern[U]): Pattern[U] =
        lhs *> rightSibling(rhs)
      infix def <*:[U](rhs: Pattern[U]): Pattern[T] =
        lhs <* rightSibling(rhs)

    extension [T](lhs: Pattern[T])
      infix def *:[U <: Tuple](rhs: Pattern[U]): Pattern[T *: U] =
        (lhs, rhs).mapN(_ *: _)
      infix def *:[U](rhs: Pattern[U])(using
          NotGiven[U <:< Tuple]
      ): Pattern[(T, U)] =
        lhs.product(rhs)

    extension [T <: Tuple](lhs: Pattern[T])
      infix def :*[U](rhs: Pattern[U]): Pattern[Tuple.Append[T, U]] =
        (lhs, rhs).mapN(_ :* _)

    extension [T](lhs: Pattern[T])(using NotGiven[T <:< Tuple])
      infix def :*[U](rhs: Pattern[U]): Pattern[(T, U)] =
        lhs.product(rhs)

    extension [T](lhs: Pattern[T])
      infix def **:[U <: Tuple](rhs: Pattern[U]): Pattern[T *: U] =
        (lhs, rightSibling(rhs)).mapN(_ *: _)
      infix def **:[U](rhs: Pattern[U])(using
          NotGiven[U <:< Tuple]
      ): Pattern[(T, U)] =
        lhs.product(rightSibling(rhs))

    extension [T <: Tuple](lhs: Pattern[T])
      infix def ++:[U <: Tuple](rhs: Pattern[U]): Pattern[Tuple.Concat[T, U]] =
        (lhs, rhs).mapN(_ ++ _)

    extension [T <: Tuple](lhs: Pattern[T])
      infix def ++*:[U <: Tuple](rhs: Pattern[U]): Pattern[Tuple.Concat[T, U]] =
        (lhs, rightSibling(rhs)).mapN(_ ++ _)

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
      private def markProgress: Pattern[T] =
        Pattern(lhs.manip.map((count, value) => (count + 1, value)))

    extension (dest: Pattern[Node.All])
      @scala.annotation.targetName("patternHere")
      def here[T](pattern: Pattern[T]): Pattern[T] =
        dest.flatMap: node =>
          Pattern(Manip.AtNode(pattern.manip, node))

    def current: Pattern[Node.All] =
      Pattern(Manip.ThisNode.map((0, _)))

    def atEnd: Pattern[Node.RightSiblingSentinel] =
      current.restrict(IsEnd)

    def anyNode: Pattern[Node] =
      current.restrict(IsNode).markProgress

    def anyParent: Pattern[Node.Parent] =
      current.restrict(IsParent).markProgress

    def anySibling: Pattern[Node.Sibling] =
      current.restrict(IsSibling).markProgress

    def theTop: Pattern[Node.Top] =
      current.restrict(IsTop)

    def anyChild: Pattern[Node.Child] =
      current.restrict(IsChild).markProgress

    def embed[T](using tag: Tag[T]): Pattern[Node.Embed[T]] =
      current.restrict(IsEmbed(tag)).markProgress

    def embedValue[T: Tag]: Pattern[T] =
      embed[T].map(_.value)

    def not(using DebugInfo)(pattern: Pattern[?]): Pattern[Unit] =
      Pattern:
        Manip
          .Negated(pattern.manip, summon[DebugInfo])
          .map(_ => (0, ()))

    def tok(using DebugInfo)(token: Token, tokens: Token*): Pattern[Node] =
      current.restrict(NodeHasToken(token, tokens*)).markProgress

    def oneOfToks(using DebugInfo)(tokens: Iterable[Token]): Pattern[Node] =
      val iter = tokens.iterator
      if !iter.hasNext
      then reject
      else
        val head = iter.next()
        current.restrict(NodeHasToken(head, iter.toSeq*)).markProgress

    def parent[T](pattern: Pattern[T]): Pattern[T] =
      anyChild.map(_.parent).here(pattern)

    def ancestor[T](pattern: Pattern[T]): Pattern[T] =
      lazy val impl: Pattern[T] =
        pattern | defer(parent(impl))

      parent(impl)

    def repeated[T](pattern: Pattern[T]): Pattern[List[T]] =
      lazy val impl: Pattern[Chain[T]] =
        (pattern *: defer(impl))
          .map(_ +: _)
          | Chain.empty.pure

      impl.map(_.toList)

    def rightSibling[T](pattern: Pattern[T]): Pattern[T] =
      anyChild.map(_.rightSibling).here(pattern)

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
        pattern <*: atEnd

    def refersToAny[T]: Pattern[List[Node]] =
      anyNode.map(_.lookup)

    def refersTo[T]: Pattern[Node] =
      refersToAny.restrict:
        case List(node) => node
  end ops
end Pattern
