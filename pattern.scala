package distcompiler

import cats.data.Chain
import cats.syntax.all.given
import scala.util.NotGiven
import izumi.reflect.Tag
import scala.reflect.TypeTest

enum Pattern[+T]:
  import Pattern.*

  case Pure(value: T)
  case Ap[T, U](ff: Pattern[T => U], fa: Pattern[T]) extends Pattern[U]
  case FlatMap[T, U](pattern: Pattern[T], fn: T => Pattern[U])
      extends Pattern[U]

  case Current extends Pattern[Node.All]
  case MarkProgress(pattern: Pattern[T])
  case AtNode(dest: Pattern[Node.All], pattern: Pattern[T])

  case Restrict[T, U](pattern: Pattern[T], fn: PartialFunction[T, U])
      extends Pattern[U]

  case Reject extends Pattern[Nothing]
  case Deferred(fn: () => Pattern[T])

  case Negation(pattern: Pattern[?]) extends Pattern[Unit]
  case Disjunction(first: Pattern[T], second: Pattern[T])

  def check(node: Node.All): Result[T] =
    import cats.Eval

    def impl[T](self: Pattern[T], node: Node.All): Eval[Result[T]] =
      self match
        case Pure(value) => Eval.now(Result.Accepted(value, 0))
        case Ap(ff, fa) =>
          impl(ff, node).flatMap:
            case Result.Rejected => Eval.now(Result.Rejected)
            case Result.Accepted(ff, matchedCount) =>
              impl(fa, node).map:
                case Result.Rejected => Result.Rejected
                case Result.Accepted(value, matchedCount2) =>
                  Result.Accepted(ff(value), matchedCount.max(matchedCount2))
        case FlatMap(pattern, fn) =>
          impl(pattern, node).flatMap:
            case Result.Rejected => Eval.now(Result.Rejected)
            case Result.Accepted(value, matchedCount) =>
              impl(fn(value), node)
                .map(_.combineMatchedCount(matchedCount))
        case Current => Eval.now(Result.Accepted(node, 0))
        case MarkProgress(pattern) =>
          impl(pattern, node).map:
            case Result.Accepted(value, matchedCount) =>
              Result.Accepted(value, matchedCount + 1)
            case Result.Rejected => Result.Rejected
        case AtNode(dest, pattern) =>
          impl(dest, node).flatMap:
            case Result.Accepted(destNode, matchedCount) =>
              impl(pattern, destNode).map:
                case Result.Accepted(value, _) =>
                  Result.Accepted(value, matchedCount)
                case Result.Rejected => Result.Rejected
            case Result.Rejected => Eval.now(Result.Rejected)
        case Restrict(pattern, fn) =>
          impl(pattern, node)
            .map:
              case Result.Rejected => Result.Rejected
              case Result.Accepted(fn(u), matchedCount) =>
                Result.Accepted(u, matchedCount)
              case Result.Accepted(_, _) => Result.Rejected
        case Reject       => Eval.now(Result.Rejected)
        case Deferred(fn) => impl(fn(), node)
        case Negation(pattern) =>
          impl(pattern, node).map:
            case Result.Rejected       => Result.Accepted((), 0)
            case Result.Accepted(_, _) => Result.Rejected
        case Disjunction(first, second) =>
          impl(first, node).flatMap:
            case accepted @ Result.Accepted(_, _) => Eval.now(accepted)
            case Result.Rejected                  => impl(second, node)

    impl(this, node).value
  end check
end Pattern

object Pattern:
  given alternative: cats.Alternative[Pattern] with
    override val unit: Pattern[Unit] = Pattern.Pure(())
    def pure[A](value: A): Pattern[A] = Pattern.Pure(value)
    def ap[A, B](ff: Pattern[A => B])(fa: Pattern[A]): Pattern[B] =
      Pattern.Ap(ff, fa)
    def empty[A]: Pattern[A] = Pattern.Reject
    def combineK[A](x: Pattern[A], y: Pattern[A]): Pattern[A] =
      Pattern.Disjunction(x, y)

  enum Result[+T]:
    case Rejected
    case Accepted[T](value: T, matchedCount: Int) extends Result[T]

    def map[U](fn: T => U): Result[U] =
      this match
        case Rejected                      => Rejected
        case Accepted(value, matchedCount) => Accepted(fn(value), matchedCount)

    def combineMatchedCount(matchedCount: Int): Result[T] =
      this match
        case Rejected => Rejected
        case Accepted(value, matchedCount2) =>
          Accepted(value, matchedCount `max` matchedCount2)

    def ignoreMatchedCount: Result[T] =
      this match
        case Rejected           => Rejected
        case Accepted(value, _) => Accepted(value, 0)

    def incMatchedCount: Result[T] =
      this match
        case Rejected                      => Rejected
        case Accepted(value, matchedCount) => Accepted(value, matchedCount + 1)
  end Result

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
      infix def |(rhs: Pattern[T]): Pattern[T] =
        Pattern.Disjunction(lhs, rhs)
      def restrict[U](fn: PartialFunction[T, U]): Pattern[U] =
        Pattern.Restrict(lhs, fn)
      def filter(fn: T => Boolean): Pattern[T] =
        lhs.restrict:
          case t if fn(t) => t
      def flatMap[U](fn: T => Pattern[U]): Pattern[U] =
        Pattern.FlatMap(lhs, fn)
      private def markProgress: Pattern[T] =
        Pattern.MarkProgress(lhs)

    extension (dest: Pattern[Node.All])
      def here[T](pattern: Pattern[T]): Pattern[T] =
        Pattern.AtNode(dest, pattern)

    def current: Pattern[Node.All] = Pattern.Current

    def atEnd: Pattern[Node.RightSiblingSentinel] =
      current.restrict(IsEnd)

    def anyNode: Pattern[Node] =
      current.restrict(IsNode).markProgress

    def anyParent: Pattern[Node.Parent] =
      current.restrict(IsParent).markProgress

    def theTop: Pattern[Node.Top] =
      current.restrict(IsTop)

    def anyChild: Pattern[Node.Child] =
      current.restrict(IsChild).markProgress

    def embed[T](using tag: Tag[T]): Pattern[Node.Embed[T]] =
      current.restrict(IsEmbed(tag)).markProgress

    def embedValue[T: Tag]: Pattern[T] =
      embed[T].map(_.value)

    def not(pattern: Pattern[?]): Pattern[Unit] = Pattern.Negation(pattern)

    def tok(token: Token, tokens: Token*): Pattern[Node] =
      current.restrict(NodeHasToken(token, tokens*)).markProgress

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

    def defer[T](pattern: => Pattern[T]): Pattern[T] =
      lazy val impl = pattern
      Pattern.Deferred(() => impl)

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
