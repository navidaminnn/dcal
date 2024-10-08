package distcompiler

import cats.syntax.all.given
import distcompiler.Node.Child
import distcompiler.Manip.ops.on.Ctx

enum Manip[+T]:
  case Backtrack(debugInfo: DebugInfo)
  case Pure(value: T)
  case Ap[T, U](ff: Manip[T => U], fa: Manip[T]) extends Manip[U]
  case FlatMap[T, U](manip: Manip[T], fn: T => Manip[U]) extends Manip[U]

  case Restrict[T, U](
      manip: Manip[T],
      pred: PartialFunction[T, U],
      debugInfo: DebugInfo
  ) extends Manip[U]

  case Effect(fn: () => T)
  case Log(debugInfo: DebugInfo, manip: Manip[T])

  case KeepLeft(left: Manip[T], right: Manip[?])
  case KeepRight(left: Manip[?], right: Manip[T])

  case Commit(manip: Manip[T], debugInfo: DebugInfo)
  case Negated(manip: Manip[?], debugInfo: DebugInfo) extends Manip[Unit]

  case RefInit[T, U](ref: Manip.Ref[T], initFn: () => T, manip: Manip[U])
      extends Manip[U]
  case RefGet[T](ref: Manip.Ref[T], debugInfo: DebugInfo) extends Manip[T]
  case RefUpdated[T, U](
      ref: Manip.Ref[T],
      fn: T => T,
      manip: Manip[U],
      debugInfo: DebugInfo
  ) extends Manip[U]

  case ThisNode extends Manip[Node.All]
  case AtNode(manip: Manip[T], node: Node.All)

  case Disjunction(first: Manip[T], second: Manip[T])
  case Deferred(fn: () => Manip[T])

  def isBacktrack: Boolean =
    this match
      case Backtrack(_) => true
      case _            => false

  def perform(using DebugInfo)(node: Node.All): T =
    import cats.Eval

    type Continue[-T, +U] = T => Eval[U]
    type Backtrack[+U] = DebugInfo => Eval[U]
    type RefMap = Map[ById[Manip.Ref[?]], Any]

    def emptyBacktrack(ctxInfo: DebugInfo, node: Node.All): Backtrack[Nothing] =
      posInfo =>
        val parentPart = node match
          case sibling: Node.Sibling => s", child of ${sibling.parent}"
          case _: Node.Root          => ""

        throw RuntimeException(
          s"unrecovered backtrack at $posInfo, caught at $ctxInfo while looking at $node$parentPart"
        )

    def impl[T, U](self: Manip[T])(using
        continue: Continue[T, U],
        backtrack: Backtrack[U],
        refMap: RefMap,
        node: Node.All
    ): Eval[U] =
      self match
        case Backtrack(debugInfo) => Eval.defer(backtrack(debugInfo))
        case Pure(value)          => Eval.defer(continue(value))
        case ap: Ap[t, u] =>
          given Continue[t => u, U] = ff =>
            given Continue[t, U] = fa => Eval.defer(continue(ff(fa)))
            Eval.defer(impl(ap.fa))
          impl(ap.ff)
        case flatMap: FlatMap[t, u] =>
          given Continue[t, U] = value =>
            given Continue[u, U] = continue
            Eval.defer(impl(flatMap.fn(value)))
          impl(flatMap.manip)
        case restrict: Restrict[t, u] =>
          given Continue[t, U] = value =>
            restrict.pred.unapply(value) match
              case None =>
                Eval.defer(backtrack(restrict.debugInfo))
              case Some(value) =>
                Eval.defer(continue(value))
          impl(restrict.manip)
        case Effect(fn) =>
          val value = fn()
          Eval.defer(continue(value))
        case Log(debugInfo, manip) =>
          val parentPart =
            node match
              case sibling: Node.Sibling => s", parent ${sibling.parent}"
              case _: Node.Root          => ""
          println(s"log: into $debugInfo, looking at node $node$parentPart")
          given Backtrack[U] = debugInfo2 =>
            println(s"log: backtrack $debugInfo because of $debugInfo2")
            Eval.defer(backtrack(debugInfo2))
          impl(manip)
        case KeepLeft(left, right: Manip[t]) =>
          given Continue[T, U] = value =>
            given Continue[t, U] = _ => Eval.defer(continue(value))
            Eval.defer(impl(right))
          impl(left)
        case KeepRight(left: Manip[t], right) =>
          given Continue[t, U] = _ =>
            given Continue[T, U] = continue
            Eval.defer(impl[T, U](right))
          impl(left)
        case Commit(manip, debugInfo) =>
          given Backtrack[U] = emptyBacktrack(debugInfo, node)
          impl(manip)
        case Negated(manip: Manip[t], debugInfo) =>
          given Backtrack[U] = _ => Eval.defer(continue(()))
          given Continue[t, U] = _ => Eval.defer(backtrack(debugInfo))
          impl(manip)
        case RefInit(ref, initFn, manip) =>
          given RefMap = refMap.updated(ById(ref), initFn())
          impl(manip)
        case RefGet(ref, debugInfo) =>
          refMap.get(ById(ref)) match
            case None        => Eval.defer(backtrack(debugInfo))
            case Some(value) => Eval.defer(continue(value.asInstanceOf[T]))
        case refUpdated: RefUpdated[t, T @unchecked] =>
          refMap.get(ById(refUpdated.ref)) match
            case None => Eval.defer(backtrack(refUpdated.debugInfo))
            case Some(value) =>
              given RefMap = refMap.updated(
                ById(refUpdated.ref),
                refUpdated.fn(value.asInstanceOf[t])
              )
              impl(refUpdated.manip)
        case ThisNode => Eval.defer(continue(node))
        case AtNode(manip, node) =>
          given Node.All = node
          impl(manip)
        case Disjunction(first, second) =>
          given Backtrack[U] = debugInfo1 =>
            given Backtrack[U] = debugInfo2 =>
              Eval.defer(backtrack(debugInfo1 ++ debugInfo2))
            Eval.defer(impl(second))
          impl(first)
        case Deferred(fn) => impl(fn())

    given Continue[T, T] = Eval.now
    given Backtrack[T] = emptyBacktrack(summon[DebugInfo], node)
    given RefMap = Map.empty
    given Node.All = node
    impl[T, T](this).value
  end perform

  // TODO: optimize disjunctions to decision tries
end Manip

object Manip:
  trait Ref[T]:
    final def init[U](init: => T)(manip: Manip[U]): Manip[U] =
      Manip.RefInit(this, () => init, manip)
    final def get(using DebugInfo): Manip[T] =
      Manip.RefGet(this, summon[DebugInfo])
    final def updated[U](using DebugInfo)(fn: T => T)(
        manip: Manip[U]
    ): Manip[U] = Manip.RefUpdated(this, fn, manip, summon[DebugInfo])

  type Rules = Manip[Node.All]

  val unit: Manip[Unit] = ().pure
  export ops.defer
  export applicative.pure

  given applicative: cats.Applicative[Manip] with
    override def unit: Manip[Unit] = Manip.unit
    def ap[A, B](ff: Manip[A => B])(fa: Manip[A]): Manip[B] =
      Manip.Ap(ff, fa)
    def pure[A](x: A): Manip[A] = Manip.Pure(x)
    override def as[A, B](fa: Manip[A], b: B): Manip[B] =
      productR(fa)(pure(b))
    override def productL[A, B](fa: Manip[A])(fb: Manip[B]): Manip[A] =
      Manip.KeepLeft(fa, fb)
    override def productR[A, B](fa: Manip[A])(fb: Manip[B]): Manip[B] =
      Manip.KeepRight(fa, fb)

  given semigroupK: cats.SemigroupK[Manip] with
    def combineK[A](x: Manip[A], y: Manip[A]): Manip[A] =
      Manip.Disjunction(x, y)

  given monoidK(using DebugInfo): cats.MonoidK[Manip] with
    export semigroupK.combineK
    def empty[A]: Manip[A] = Manip.Backtrack(summon[DebugInfo])

  class Lookahead[T](val manip: Manip[T]) extends AnyVal:
    def flatMap[U](fn: T => Manip[U]): Manip[U] =
      Manip.FlatMap(manip, fn)

  object Lookahead:
    given applicative: cats.Applicative[Lookahead] with
      def ap[A, B](ff: Lookahead[A => B])(fa: Lookahead[A]): Lookahead[B] =
        Lookahead(ff.manip.ap(fa.manip))
      def pure[A](x: A): Lookahead[A] =
        Lookahead(Manip.Pure(x))
    given semigroupK: cats.SemigroupK[Lookahead] with
      def combineK[A](x: Lookahead[A], y: Lookahead[A]): Lookahead[A] =
        Lookahead(x.manip.combineK(y.manip))
    given monoidK(using DebugInfo): cats.MonoidK[Lookahead] with
      export semigroupK.combineK
      def empty[A]: Lookahead[A] =
        Lookahead(Manip.Backtrack(summon[DebugInfo]))

  object ops:
    def defer[T](manip: => Manip[T]): Manip[T] =
      lazy val impl = manip
      Manip.Deferred(() => impl)

    def commit[T](using DebugInfo)(manip: Manip[T]): Manip[T] =
      Manip.Commit(manip, summon[DebugInfo])

    def effect[T](fn: => T): Manip[T] =
      Manip.Effect(() => fn)

    def backtrack(using DebugInfo): Manip[Nothing] =
      Manip.Backtrack(summon[DebugInfo])

    def log[T](using DebugInfo)(manip: Manip[T]): Manip[T] =
      Manip.Log(summon[DebugInfo], manip)

    def atNode[T](node: Node.All)(manip: Manip[T]): Manip[T] =
      Manip.AtNode(manip, node)

    def atRightSibling[T](using DebugInfo)(manip: Manip[T]): Manip[T] =
      Manip.ThisNode.lookahead.flatMap:
        case thisChild: Node.Child =>
          atNode(thisChild.rightSibling)(manip)
        case _: (Node.Sentinel | Node.Root) =>
          Manip.Backtrack(summon[DebugInfo])

    def atFirstChild[T](using DebugInfo)(manip: Manip[T]): Manip[T] =
      Manip.ThisNode.lookahead.flatMap:
        case thisParent: Node.Parent =>
          atNode(thisParent.firstChild)(manip)
        case _: (Node.Sentinel | Node.Leaf) =>
          Manip.Backtrack(summon[DebugInfo])

    def atFirstSibling[T](using DebugInfo)(manip: Manip[T]): Manip[T] =
      atParent(atFirstChild(manip))

    def atParent[T](using DebugInfo)(manip: Manip[T]): Manip[T] =
      Manip.ThisNode.lookahead.flatMap:
        case thisChild: Node.Sibling =>
          atNode(thisChild.parent)(manip)
        case _: Node.Root =>
          Manip.Backtrack(summon[DebugInfo])

    def addChild[T](using DebugInfo)(child: => Node.Child): Manip[Node.Child] =
      Manip.ThisNode.flatMap:
        case thisParent: Node.Parent =>
          effect:
            val tmp = child
            thisParent.children.addOne(tmp)
            tmp
        case _ => Manip.Backtrack(summon[DebugInfo])

    final class on[T](val pattern: Pattern[T]):
      def raw: Manip[(Int, T)] =
        pattern.manip

      def value(using DebugInfo): Manip[T] =
        raw.map(_._2)

      def check(using DebugInfo): Manip[Unit] =
        raw.as(())

      def rewrite(using DebugInfo)(
          action: on.Ctx ?=> T => Rules
      ): Rules =
        raw
          .product(Manip.ThisNode)
          .flatMap:
            case ((matchedCount, value), node) =>
              action(using on.Ctx(matchedCount, node.asSibling.parent, node.asSibling.idxInParent))(value)

    object on:
      final case class Ctx(count: Int, parent: Node.Parent, idxInParent: Int)

    transparent trait SpliceOps:
      final def apply(using on.Ctx)(nodes: Node.Child*): Rules =
        rules(nodes)

      final def apply(using on.Ctx)(nodes: IterableOnce[Node.Child]): Rules =
        rules(nodes.iterator.toArray)

      protected def rules(using ctx: on.Ctx)(
          nodes: Iterable[Node.Child]
      ): Rules =
        effect:
          val parent = ctx.parent
          val idx = ctx.idxInParent
          parent.children.patchInPlace(idx, nodes, ctx.count)
          parent.children.findSibling(idx + nodes.size)

    object Splice extends SpliceOps

    object SpliceAndRetry extends SpliceOps:
      override protected def rules(using
          ctx: on.Ctx
      )(nodes: Iterable[Node.Child]): Rules =
        super.rules(nodes).as(ctx.parent.children.findSibling(ctx.idxInParent))

    object SpliceAndFirstChild extends SpliceOps:
      override protected def rules(using
          ctx: Ctx
      )(nodes: Iterable[Child]): Rules =
        super.rules(nodes)
          .as(ctx.parent.children.findSibling(ctx.idxInParent).asNode.firstChild)

    def SkipMatch(using ctx: on.Ctx): Rules =
      pure(ctx.parent.children.findSibling(ctx.idxInParent + ctx.count))

    def Goto(node: Node.All): Rules =
      pure(node)

    extension [T](lhs: Manip[T])
      @scala.annotation.targetName("or")
      def |(rhs: Manip[T]): Manip[T] =
        Manip.Disjunction(lhs, rhs)
      def flatMap[U](using DebugInfo)(fn: T => Manip[U]): Manip[U] =
        Manip.FlatMap(lhs, t => commit(fn(t)))
      def lookahead: Lookahead[T] =
        Lookahead(lhs)
      def restrict[U](using DebugInfo)(fn: PartialFunction[T, U]): Manip[U] =
        Manip.Restrict(lhs, fn, summon[DebugInfo])

    extension (lhs: Manip[Node.All])
      def here[U](manip: Manip[U]): Manip[U] =
        lhs.lookahead.flatMap: node =>
          atNode(node)(manip)

    // TODO: debug info as implicit param.
    // - optionally print AST after pass
    // - for initial AST, give direct option to print that in reader
    // - include link to file + line num in printed info
    // - option to print after every edit?
    // - don't discount ability to make debug adapter for this later...

    final class pass(
        strategy: pass.TraversalStrategy = pass.topDown,
        once: Boolean = false,
        wrapFn: Manip[Unit] => Manip[Unit] = identity
    ):
      def rules(using DebugInfo)(rules: Rules): Manip[Unit] =
        val tracer = pass.tracerRef.get | pure(pass.NopTracer)
        val before = tracer
          .product(Manip.ThisNode)
          .flatMap: (tracer, node) =>
            effect(tracer.beforePass(summon[DebugInfo], node))
        val after = tracer
          .product(Manip.ThisNode)
          .flatMap: (tracer, node) =>
            effect(tracer.afterPass(summon[DebugInfo], node))

        lazy val impl: Manip[Unit] =
          before
          *> strategy
            .traverse(rules)
            .flatMap: madeChange =>
              after
                *> (if madeChange && !once
                    then impl
                    else Manip.unit)

        wrapFn(impl)

      def withState[T](ref: Ref[T])(init: => T): pass =
        pass(
          strategy = strategy,
          once = once,
          wrapFn = manip => ref.init(init)(defer(wrapFn(manip)))
        )
    end pass

    object pass:
      object tracerRef extends Ref[Tracer]

      trait Tracer:
        def beforePass(debugInfo: DebugInfo, tree: Node.All): Unit
        def afterPass(debugInfo: DebugInfo, tree: Node.All): Unit
        // def afterRewrite(debugInfo: DebugInfo, tree: Node.All): Unit

      object NopTracer extends Tracer:
        def beforePass(debugInfo: DebugInfo, tree: Node.All): Unit = ()
        def afterPass(debugInfo: DebugInfo, tree: Node.All): Unit = ()

      trait TraversalStrategy:
        def traverse(rules: Rules): Manip[Boolean]

      object topDown extends TraversalStrategy:
        def traverse(
            rules: Rules
        ): Manip[Boolean] =
          lazy val impl: Manip[Boolean] =
            commit:
              rules.flatMap: nextNode =>
                commit:
                  atNode(nextNode):
                    impl.as(true)
              | atFirstChild(defer(impl))
                | atRightSibling(defer(impl))
                | atParent(atRightSibling(defer(impl)))
                | atParent(on(Pattern.ops.theTop).check.as(false))

          impl

      object bottomUp extends TraversalStrategy:
        def traverse(
            rules: Rules
        ): Manip[Boolean] =
          // TODO: fix this code, not tested and probably has similar bug to what topDown had
          def atBottomLeft[T](manip: Manip[T]): Manip[T] =
            lazy val impl: Manip[T] =
              atFirstChild(defer(impl))
                | manip

            impl

          lazy val impl: Manip[Boolean] =
            rules.flatMap: nextNode =>
              atNode(nextNode):
                impl.as(true)
            | atRightSibling:
              commit(defer(impl))
            | atParent:
              atRightSibling:
                atFirstChild:
                  commit(defer(impl))
              | atFirstSibling:
                commit(defer(impl))
            | atParent(on(Pattern.ops.theTop).check.as(false))

          atBottomLeft(impl)
  end ops
end Manip
