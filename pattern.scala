package distcompiler

import cats.data.Chain
import cats.syntax.all.given
import scala.util.NotGiven
import scala.reflect.TypeTest

enum Pattern[+T]:
  import Pattern.*

  case Pure(value: T)
  case Ap[T, U](ff: Pattern[T => U], fa: Pattern[T]) extends Pattern[U]
  case FlatMap[T, U](pattern: Pattern[T], fn: T => Pattern[U])
      extends Pattern[U]

  case ThisChild extends Pattern[Node.Child]
  case ThisEmbed[T](typeTest: TypeTest[Any, T]) extends Pattern[Node.Embed[T]]
  case ThisNode extends Pattern[Node]
  case AtEnd extends Pattern[Unit]
  case AtParent(pattern: Pattern[T])
  case AtRightSibling(pattern: Pattern[T])
  case AtFirstChild(pattern: Pattern[T])
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
        case ThisChild =>
          node match
            case child: Node.Child => Eval.now(Result.Accepted(child, 1))
            case _ => Eval.now(Result.Rejected)
        case thisEmbed: ThisEmbed[t] =>
          given TypeTest[Any, t] = thisEmbed.typeTest
          node match
            case embed: Node.Embed[`t`] if embed.value.isInstanceOf[t] =>
              Eval.now(Result.Accepted(embed, 1))
            case _ => Eval.now(Result.Rejected)
        case ThisNode =>
          node match
            case node: Node => Eval.now(Result.Accepted(node, 1))
            case _          => Eval.now(Result.Rejected)
        case AtEnd =>
          node match
            case _: Node.RightSiblingSentinel =>
              Eval.now(Result.Accepted((), 0))
            case _ => Eval.now(Result.Rejected)
        case AtParent(pattern) =>
          node match
            case sibling: Node.Sibling =>
              impl(pattern, sibling.parent)
                .map(_.ignoreMatchedCount)
            case _ => Eval.now(Result.Rejected)
        case AtRightSibling(pattern) =>
          node match
            case node: Node.Child =>
              impl(pattern, node.rightSibling)
                .map(_.incMatchedCount)
            case _ => Eval.now(Result.Rejected)
        case AtFirstChild(pattern) =>
          node match
            case node: Node.Parent =>
              impl(pattern, node.firstChild)
                .map(_.ignoreMatchedCount)
            case _ =>
              Eval.now(Result.Rejected)
        case AtNode(dest, pattern) =>
          impl(dest, node).flatMap:
            case Result.Accepted(destNode, matchedCount) =>
              impl(pattern, destNode).map:
                case Result.Accepted(value, _) => Result.Accepted(value, matchedCount)
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
  // TODO: refersTo operation, don't bother w/ native...
  // - version that assumes single ref?
  // - refersTo: Pattern[List[Node]], gives you all the refs to do _something_ with

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

  extension [T](lhs: Pattern[T])
    infix def **:[U <: Tuple](rhs: Pattern[U]): Pattern[T *: U] =
      (lhs, rightSibling(rhs)).mapN(_ *: _)
    infix def **:[U](rhs: Pattern[U])(using
        NotGiven[U <:< Tuple]
    ): Pattern[(T, U)] =
      lhs.product(rightSibling(rhs))

  extension [T](lhs: Pattern[T])
    infix def |(rhs: Pattern[T]): Pattern[T] =
      Pattern.Disjunction(lhs, rhs)
    def restrict[U](fn: PartialFunction[T, U]): Pattern[U] =
      Pattern.Restrict(lhs, fn)
    def flatMap[U](fn: T => Pattern[U]): Pattern[U] =
      Pattern.FlatMap(lhs, fn)
  
  extension (dest: Pattern[Node.All])
    def here[T](pattern: Pattern[T]): Pattern[T] =
      Pattern.AtNode(dest, pattern)

  def atEnd: Pattern[Unit] = Pattern.AtEnd

  def anyTok: Pattern[Node] = Pattern.ThisNode

  def anyChild: Pattern[Node.Child] = Pattern.ThisChild

  def embed[T](using typeTest: TypeTest[Any, T]): Pattern[Node.Embed[T]] =
    Pattern.ThisEmbed(typeTest)

  def embedValue[T](using TypeTest[Any, T]): Pattern[T] =
    embed[T].map(_.value)

  def pure[T](value: T): Pattern[T] = Pattern.Pure(value)

  def tok(token: Token, tokens: Token*): Pattern[Node] =
    anyTok.restrict:
      case node if node.token == token         => node
      case node if tokens.contains(node.token) => node

  def parent[T](pattern: Pattern[T]): Pattern[T] =
    Pattern.AtParent(pattern)

  def ancestor[T](pattern: Pattern[T]): Pattern[T] =
    lazy val impl: Pattern[T] =
      pattern | defer(parent(impl))

    parent(impl)

  def repeated[T](pattern: Pattern[T]): Pattern[List[T]] =
    lazy val impl: Pattern[Chain[T]] =
      (pattern *: defer(impl))
        .map(_ +: _)
        | pure(Chain.empty)

    impl.map(_.toList)

  def rightSibling[T](pattern: Pattern[T]): Pattern[T] =
    Pattern.AtRightSibling(pattern)

  def firstChild[T](pattern: Pattern[T]): Pattern[T] =
    Pattern.AtFirstChild(pattern)

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

  def refersTo[T](pattern: Pattern[T]): Pattern[T] =
    anyTok
      .map(_.lookup)
      .restrict:
        case List(singleResult) => singleResult
      .here(pattern)

  def refersToRelative[T](relativeTo: Pattern[Node])(pattern: Pattern[T]): Pattern[T] =
    (anyTok, relativeTo)
      .mapN: (thisNode, relativeTo) =>
        thisNode.lookupRelativeTo(relativeTo)
      .restrict:
        case List(singleResult) => singleResult
      .here(pattern)
end Pattern
