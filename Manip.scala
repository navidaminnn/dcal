package distcompiler

import cats.syntax.all.given

enum Manip[+T]:
  case Backtrack(debugInfo: DebugInfo)
  case Pure(value: T)
  case Ap[T, U](ff: Manip[T => U], fa: Manip[T]) extends Manip[U]
  case FlatMap[T, U](manip: Manip[T], fn: T => Manip[U]) extends Manip[U]
  case Effect(manip: Manip[T])

  case Commit(manip: Manip[T], debugInfo: DebugInfo)

  case RefInit[T, U](ref: Manip.Ref[T], init: T, manip: Manip[U])
      extends Manip[U]
  case RefGet[T](ref: Manip.Ref[T], debugInfo: DebugInfo) extends Manip[T]
  case RefUpdated[T, U](
      ref: Manip.Ref[T],
      fn: T => T,
      manip: Manip[U],
      debugInfo: DebugInfo
  ) extends Manip[U]

  case ThisNode extends Manip[Node.All]
  case OnPattern[T](pattern: Pattern[T], debugInfo: DebugInfo)
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
      case Backtrack(debugInfo: DebugInfo)
    end Result
    import Result.Ok

    def impl[T](self: Manip[T], node: Node.All)(using
        refs: Map[ById[Manip.Ref[?]], Any]
    ): Eval[Result[T]] =
      self match
        case Manip.Backtrack(debugInfo) => Eval.now(Result.Backtrack(debugInfo))
        case Pure(value)                => Eval.now(Ok(value))
        case Ap(ff, fa) =>
          impl(ff, node).flatMap:
            case Ok(ff) =>
              impl(fa, node).map:
                case Ok(fa)                                   => Ok(ff(fa))
                case bt: Result.Backtrack[Nothing @unchecked] => bt
            case bt: Result.Backtrack[Nothing @unchecked] => Eval.now(bt)
        case FlatMap(manip, fn) =>
          impl(manip, node).flatMap:
            case Ok(value) =>
              impl(fn(value), node)
            case bt: Result.Backtrack[Nothing @unchecked] => Eval.now(bt)
        case Effect(manip) => impl(manip, node)
        case Commit(manip, debugInfo1) =>
          impl(manip, node).map:
            case ok @ Ok(_) => ok
            case Result.Backtrack(debugInfo2) =>
              throw RuntimeException(
                s"unrecovered backtrack at $debugInfo2, caught at $debugInfo1"
              )
        case RefInit(ref, init, manip) =>
          impl(manip, node)(using refs.updated(ById(ref), init))
        case refGet: RefGet[t] =>
          refs.get(ById(refGet.ref)) match
            case None        => Eval.now(Result.Backtrack(refGet.debugInfo))
            case Some(value) => Eval.now(Ok(value.asInstanceOf[t]))
        case refUpdated: RefUpdated[t, u] =>
          refs.get(ById(refUpdated.ref)) match
            case None => Eval.now(Result.Backtrack(refUpdated.debugInfo))
            case Some(value) =>
              impl(refUpdated.manip, node)(using
                refs.updated(
                  ById(refUpdated.ref),
                  refUpdated.fn(value.asInstanceOf[t])
                )
              )
        case ThisNode => Eval.now(Ok(node))
        case onPattern: OnPattern[t] =>
          onPattern.pattern.check(node) match
            case accepted: Pattern.Result.Accepted[`t`] =>
              Eval.now(Ok(accepted))
            case Pattern.Result.Rejected =>
              Eval.now(Result.Backtrack(onPattern.debugInfo))
        case AtNode(manip, node) => impl(manip, node)
        case Disjunction(first, second) =>
          impl(first, node).flatMap:
            case ok @ Ok(_) => Eval.now(ok)
            case bt: Result.Backtrack[Nothing @unchecked] =>
              impl(second, node).map:
                case ok @ Ok(_) => ok
                case bt2: Result.Backtrack[Nothing @unchecked] =>
                  Result.Backtrack(bt.debugInfo ++ bt2.debugInfo)
        case Deferred(fn) =>
          impl(fn(), node)

    impl(this, top)(using Map.empty).value match
      case bt: Result.Backtrack[?] =>
        throw RuntimeException(s"unrecovered backtrack at ${bt.debugInfo}")
      case Ok(value) => value
  end perform

  // TODO: optimize disjunctions to decision tries
end Manip

object Manip:
  trait Ref[T]:
    final def init[U](init: T)(manip: Manip[U]): Manip[U] =
      Manip.RefInit(this, init, manip)
    final def get(using DebugInfo): Manip[T] =
      Manip.RefGet(this, summon[DebugInfo])
    final def updated[U](using DebugInfo)(fn: T => T)(
        manip: Manip[U]
    ): Manip[U] = Manip.RefUpdated(this, fn, manip, summon[DebugInfo])

  type Rules = Manip[(Node.Sibling, Node.Sibling)]

  val unit: Manip[Unit] = ().pure
  export ops.defer
  export applicative.pure

  given applicative: cats.Applicative[Manip] with
    override def unit: Manip[Unit] = Manip.unit
    def ap[A, B](ff: Manip[A => B])(fa: Manip[A]): Manip[B] =
      Manip.Ap(ff, fa)
    def pure[A](x: A): Manip[A] = Manip.Pure(x)

  given monoidK(using DebugInfo): cats.MonoidK[Manip] with
    def combineK[A](x: Manip[A], y: Manip[A]): Manip[A] =
      Manip.Disjunction(x, y)
    def empty[A]: Manip[A] = Manip.Backtrack(summon[DebugInfo])

  object ops:
    def defer[T](manip: => Manip[T]): Manip[T] =
      lazy val impl = manip
      Manip.Deferred(() => impl)

    def commit[T](using DebugInfo)(manip: Manip[T]): Manip[T] =
      Manip.Commit(manip, summon[DebugInfo])

    def atNode[T](node: Node.All)(manip: Manip[T]): Manip[T] =
      Manip.AtNode(manip, node)

    def atRightSibling[T](using DebugInfo)(manip: Manip[T]): Manip[T] =
      Manip.ThisNode.lookahead.flatMap:
        case thisChild: Node.Child =>
          atNode(thisChild)(manip)
        case _: (Node.Sentinel | Node.Root) =>
          Manip.Backtrack(summon[DebugInfo])

    def atFirstChild[T](using DebugInfo)(manip: Manip[T]): Manip[T] =
      Manip.ThisNode.lookahead.flatMap:
        case thisParent: Node.Parent =>
          atNode(thisParent.firstChild)(manip)
        case _: (Node.Sentinel | Node.Leaf) =>
          Manip.Backtrack(summon[DebugInfo])

    def atFirstSibling[T](using DebugInfo)(manip: Manip[T]): Manip[T] =
      Manip.ThisNode.lookahead.flatMap:
        case thisSibling: Node.Sibling =>
          atNode(thisSibling.parent.children.findSibling(0))(manip)
        case _: Node.Root =>
          Manip.Backtrack(summon[DebugInfo])

    def atParent[T](using DebugInfo)(manip: Manip[T]): Manip[T] =
      Manip.ThisNode.lookahead.flatMap:
        case thisChild: Node.Sibling =>
          atNode(thisChild.parent)(manip)
        case _: Node.Root =>
          Manip.Backtrack(summon[DebugInfo])

    def addChild[T](using DebugInfo)(child: => Node.Child): Manip[Node.Child] =
      Manip.ThisNode.effect:
        case thisParent: Node.Parent =>
          val tmp = child
          thisParent.children.addOne(tmp)
          pure(tmp)
        case _ => Manip.Backtrack(summon[DebugInfo])

    final class on[T](val pattern: Pattern[T]) extends AnyVal:
      def raw(using DebugInfo): Manip[Pattern.Result.Accepted[T]] =
        Manip.OnPattern(pattern, summon[DebugInfo])

      def value(using DebugInfo): Manip[T] =
        raw.map(_.value)

      def check(using DebugInfo): Manip[Unit] =
        raw.as(())

      def rewrite(using DebugInfo)(
          action: T => RewriteOp
      ): Manip[(Node.Sibling, Node.Sibling)] =
        raw.flatMap:
          case Pattern.Result.Accepted(value, matchedCount) =>
            Manip.ThisNode.effect: thisNode =>
              val replacementsOpt
                  : Manip.Backtrack[Nothing] | Skip.type | Iterable[
                    Node.Child
                  ] =
                action(value) match
                  case node: Node.Child => node :: Nil
                  case Splice(nodes*)   => nodes
                  case Delete           => Nil
                  case tn @ TryNext()   => Manip.Backtrack(tn.debugInfo)
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
                case bt: Manip.Backtrack[Nothing] => bt
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
      def flatMap[U](using DebugInfo)(fn: T => Manip[U]): Manip[U] =
        Manip.FlatMap(lhs, t => commit(fn(t)))
      def effect[U](fn: T => Manip[U]): Manip[U] =
        lhs.flatMap(fn)
      def lookahead: Lookahead[T] =
        Lookahead(lhs)

    extension (lhs: Manip[Node.All])
      def here[U](manip: Manip[U]): Manip[U] =
        lhs.lookahead.flatMap: node =>
          atNode(node)(manip)

    class Lookahead[T](val manip: Manip[T]) extends AnyVal:
      def flatMap[U](fn: T => Manip[U]): Manip[U] =
        Manip.FlatMap(manip, fn)
      def effect[U](fn: T => Manip[U]): Manip[U] =
        flatMap(t => Manip.Effect(fn(t)))

    object Lookahead:
      given applicative: cats.Applicative[Lookahead] with
        def ap[A, B](ff: Lookahead[A => B])(fa: Lookahead[A]): Lookahead[B] =
          Lookahead(ff.manip.ap(fa.manip))
        def pure[A](x: A): Lookahead[A] =
          Lookahead(Manip.Pure(x))
      given monoidK(using DebugInfo): cats.MonoidK[Lookahead] with
        def combineK[A](x: Lookahead[A], y: Lookahead[A]): Lookahead[A] =
          Lookahead(x.manip.combineK(y.manip))
        def empty[A]: Lookahead[A] =
          Lookahead(Manip.Backtrack(summon[DebugInfo]))

    final case class Splice(nodes: Node.Child*)
    case object Delete
    final case class TryNext()(using val debugInfo: DebugInfo)
    case object Skip

    type RewriteOp =
      Node.Child | Splice | Delete.type | TryNext | Skip.type

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
