package distcompiler

import cats.syntax.all.given

enum Manip[+T]:
  case Backtrack
  case Pure(value: T)
  case Ap[T, U](ff: Manip[T => U], fa: Manip[T]) extends Manip[U]
  case FlatMap[T, U](manip: Manip[T], fn: T => Manip[U]) extends Manip[U]

  case Commit(manip: Manip[T])

  case ThisNode extends Manip[Node.All]
  case OnPattern[T](pattern: Pattern[T])
      extends Manip[Pattern.Result.Accepted[T]]
  case AtNode(manip: Manip[T], node: Node.All)

  case Disjunction(first: Manip[T], second: Manip[T])
  case Deferred(fn: () => Manip[T])

  def perform(top: Node.Top): T =
    // TODO: optimize so we use Commit for performance (discard backtrack info in that case)
    // this would either work via continuation passing, or by taking a more imperative approach
    // and managing a mutable stack type structure directly
    import cats.Eval

    enum Result[+T]:
      case Ok(value: T)
      case Backtrack
    end Result
    import Result.Ok

    def impl[T](self: Manip[T], node: Node.All): Eval[Result[T]] =
      self match
        case Manip.Backtrack => Eval.now(Result.Backtrack)
        case Pure(value)     => Eval.now(Ok(value))
        case Ap(ff, fa) =>
          impl(ff, node).flatMap:
            case Ok(ff) =>
              impl(fa, node).map:
                case Ok(fa)           => Ok(ff(fa))
                case Result.Backtrack => Result.Backtrack
            case Result.Backtrack => Eval.now(Result.Backtrack)
        case FlatMap(manip, fn) =>
          impl(manip, node).flatMap:
            case Ok(value) =>
              impl(fn(value), node)
            case Result.Backtrack => Eval.now(Result.Backtrack)
        case Commit(manip) =>
          impl(manip, node).map:
            case ok @ Ok(_) => ok
            case Result.Backtrack =>
              throw RuntimeException(
                "tried to backtrack after committing to a branch"
              )
        case ThisNode => Eval.now(Ok(node))
        case onPattern: OnPattern[t] =>
          onPattern.pattern.check(node) match
            case accepted: Pattern.Result.Accepted[`t`] =>
              Eval.now(Ok(accepted))
            case Pattern.Result.Rejected =>
              Eval.now(Result.Backtrack)
        case AtNode(manip, node) => impl(manip, node)
        case Disjunction(first, second) =>
          impl(first, node).flatMap:
            case ok @ Ok(_) => Eval.now(ok)
            case Result.Backtrack =>
              impl(second, node)
        case Deferred(fn) =>
          impl(fn(), node)

    impl(this, top).value match
      case Result.Backtrack =>
        throw RuntimeException("tree manipulation backtracked at top level")
      case Ok(value) => value
  end perform

  // TODO: optimize disjunctions to decision tries
end Manip

object Manip:
  type Rules = Manip[(Node.Sibling, Node.Sibling)]

  val unit: Manip[Unit] = ().pure

  given alternative: cats.Alternative[Manip] with
    override def unit: Manip[Unit] = Manip.unit
    def ap[A, B](ff: Manip[A => B])(fa: Manip[A]): Manip[B] =
      Manip.Ap(ff, fa)
    def combineK[A](x: Manip[A], y: Manip[A]): Manip[A] =
      Manip.Disjunction(x, y)
    def empty[A]: Manip[A] = Manip.Backtrack
    def pure[A](x: A): Manip[A] = Manip.Pure(x)

  object ops:
    def defer[T](manip: => Manip[T]): Manip[T] =
      lazy val impl = manip
      Manip.Deferred(() => impl)

    def commit[T](manip: Manip[T]): Manip[T] =
      Manip.Commit(manip)

    def atNode[T](node: Node.All)(manip: Manip[T]): Manip[T] =
      Manip.AtNode(manip, node)

    def atRightSibling[T](manip: Manip[T]): Manip[T] =
      Manip.ThisNode.lookahead.flatMap:
        case thisChild: Node.Child =>
          atNode(thisChild)(manip)
        case _: (Node.Sentinel | Node.Root) =>
          Manip.Backtrack

    def atFirstChild[T](manip: Manip[T]): Manip[T] =
      Manip.ThisNode.lookahead.flatMap:
        case thisParent: Node.Parent =>
          atNode(thisParent.firstChild)(manip)
        case _: (Node.Sentinel | Node.Leaf) =>
          Manip.Backtrack

    def atFirstSibling[T](manip: Manip[T]): Manip[T] =
      Manip.ThisNode.lookahead.flatMap:
        case thisSibling: Node.Sibling =>
          atNode(thisSibling.parent.children.findSibling(0))(manip)
        case _: Node.Root =>
          Manip.Backtrack

    def atParent[T](manip: Manip[T]): Manip[T] =
      Manip.ThisNode.lookahead.flatMap:
        case thisChild: Node.Sibling =>
          atNode(thisChild.parent)(manip)
        case _: Node.Root =>
          Manip.Backtrack

    final class on[T](val pattern: Pattern[T]) extends AnyVal:
      def raw: Manip[Pattern.Result.Accepted[T]] =
        Manip.OnPattern(pattern)

      def value: Manip[T] =
        raw.map(_.value)

      def rewrite(action: T => RewriteOp): Manip[(Node.Sibling, Node.Sibling)] =
        raw.flatMap:
          case Pattern.Result.Accepted(value, matchedCount) =>
            Manip.ThisNode.effect: thisNode =>
              val replacementsOpt
                  : Manip.Backtrack.type | Skip.type | Iterable[Node.Child] =
                action(value) match
                  case node: Node.Child => node :: Nil
                  case Splice(nodes*)   => nodes
                  case Delete           => Nil
                  case TryNext          => Backtrack
                  case Skip             => Skip

              replacementsOpt match
                case Skip =>
                  thisNode match
                    case thisChild: Node.Child =>
                      (thisChild, thisChild.rightSibling).pure
                    case _: (Node.Sentinel | Node.Root) =>
                      throw RuntimeException(
                        "tried to continue at sentinel or root"
                      )
                case Manip.Backtrack => Manip.Backtrack
                case replacements: Iterable[Node.Child] =>
                  thisNode match
                    case thisSibling: Node.Sibling =>
                      val parent = thisSibling.parent
                      val startIdx = thisSibling.idxInParent
                      thisSibling.parent.children.patchInPlace(
                        startIdx,
                        replacements,
                        matchedCount
                      )
                      // two choices: stay where we are, or jump to the next untouched node
                      (
                        parent.children.findSibling(startIdx),
                        parent.children.findSibling(
                          startIdx + replacements.size
                        )
                      ).pure
                    case thisRoot: Node.Root =>
                      throw RuntimeException("tried to rewrite root node")

    extension [T](lhs: Manip[T])
      def |(rhs: Manip[T]): Manip[T] =
        Manip.Disjunction(lhs, rhs)
      def flatMap[U](fn: T => Manip[U]): Manip[U] =
        Manip.FlatMap(lhs, t => commit(fn(t)))
      def effect[U](fn: T => Manip[U]): Manip[U] =
        lhs.flatMap(fn)
      def lookahead: Lookahead[T] =
        Lookahead(lhs)

    class Lookahead[T](val manip: Manip[T]) extends AnyVal:
      def flatMap[U](fn: T => Manip[U]): Manip[U] =
        Manip.FlatMap(manip, fn)

    object Lookahead:
      given alternative: cats.Alternative[Lookahead] with
        def ap[A, B](ff: Lookahead[A => B])(fa: Lookahead[A]): Lookahead[B] =
          Lookahead(ff.manip.ap(fa.manip))
        def combineK[A](x: Lookahead[A], y: Lookahead[A]): Lookahead[A] =
          Lookahead(x.manip.combineK(y.manip))
        def empty[A]: Lookahead[A] =
          Lookahead(Manip.Backtrack)
        def pure[A](x: A): Lookahead[A] =
          Lookahead(Manip.Pure(x))

    final case class Splice(nodes: Node.Child*)
    case object Delete
    case object TryNext
    case object Skip

    type RewriteOp =
      Node.Child | Splice | Delete.type | TryNext.type | Skip.type

    final class pass(
        strategy: pass.TraversalStrategy = pass.topDown,
        once: Boolean = false
    ):
      def rules(rules: Manip[(Node.Sibling, Node.Sibling)]): Manip[Unit] =
        lazy val impl: Manip[Unit] =
          strategy
            .traverse(rules)
            .flatMap: madeChange =>
              if madeChange && !once
              then impl
              else Manip.unit

        impl
    end pass

    object pass:
      trait TraversalStrategy:
        def traverse(rules: Manip[(Node.Sibling, Node.Sibling)]): Manip[Boolean]

      object topDown extends TraversalStrategy:
        def traverse(
            rules: Manip[(Node.Sibling, Node.Sibling)]
        ): Manip[Boolean] =
          lazy val impl: Manip[Boolean] =
            rules.flatMap: (_, nextSibling) =>
              atNode(nextSibling):
                impl.as(true)
            | atFirstChild:
              commit(defer(impl))
            | atRightSibling:
              commit(defer(impl))
            | atParent:
              atRightSibling:
                commit(defer(impl))
            | false.pure

          impl

      object bottomUp extends TraversalStrategy:
        def traverse(
            rules: Manip[(Node.Sibling, Node.Sibling)]
        ): Manip[Boolean] =
          def atBottomLeft[T](manip: Manip[T]): Manip[T] =
            lazy val impl: Manip[T] =
              atFirstChild(defer(impl))
                | manip

            impl

          lazy val impl: Manip[Boolean] =
            rules.flatMap: (_, nextSibling) =>
              atNode(nextSibling):
                impl.as(true)
            | atRightSibling:
              commit(defer(impl))
            | atParent:
              atRightSibling:
                atFirstChild:
                  commit(defer(impl))
              | atFirstSibling:
                commit(defer(impl))
            | false.pure

          atBottomLeft(impl)
  end ops
end Manip
