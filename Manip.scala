package distcompiler

import cats.syntax.all.given

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
  case Finally(manip: Manip[T], fn: () => Unit)

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
    import scala.util.control.TailCalls.*

    type Continue[-T, +U] = T => TailRec[U]
    type Backtrack[+U] = DebugInfo => TailRec[U]
    type RefMap = Map[ById[Manip.Ref[?]], Any]

    def emptyBacktrack(
        ctxInfo: DebugInfo,
        node: Node.All,
        refMap: RefMap
    ): Backtrack[Nothing] =
      posInfo =>
        refMap.get(ById(Manip.tracerRef)) match
          case None =>
          case Some(tracer) =>
            tracer
              .asInstanceOf[Manip.Tracer]
              .onFatal(ctxInfo, posInfo, node)

        throw RuntimeException(
          s"unrecovered backtrack at $posInfo, caught at $ctxInfo while looking at $node"
        )

    def impl[T, U](self: Manip[T])(using
        continue: Continue[T, U],
        backtrack: Backtrack[U],
        refMap: RefMap,
        node: Node.All
    ): TailRec[U] =
      self match
        case Backtrack(debugInfo) => tailcall(backtrack(debugInfo))
        case Pure(value)          => tailcall(continue(value))
        case ap: Ap[t, u] =>
          given Continue[t => u, U] = ff =>
            given Continue[t, U] = fa => tailcall(continue(ff(fa)))
            tailcall(impl(ap.fa))
          impl(ap.ff)
        case flatMap: FlatMap[t, u] =>
          given Continue[t, U] = value =>
            given Continue[u, U] = continue
            tailcall(impl(flatMap.fn(value)))
          impl(flatMap.manip)
        case restrict: Restrict[t, u] =>
          given Continue[t, U] = value =>
            restrict.pred.unapply(value) match
              case None =>
                tailcall(backtrack(restrict.debugInfo))
              case Some(value) =>
                tailcall(continue(value))
          impl(restrict.manip)
        case Effect(fn) =>
          val value = fn()
          tailcall(continue(value))
        case Log(debugInfo, manip) =>
          println(s"log: into $debugInfo, looking at node $node")
          given Backtrack[U] = debugInfo2 =>
            println(s"log: backtrack $debugInfo because of $debugInfo2")
            tailcall(backtrack(debugInfo2))
          impl(manip)
        case Finally(manip, fn) =>
          given Backtrack[U] = info =>
            fn()
            tailcall(backtrack(info))
          given Continue[T, U] = value =>
            fn()
            tailcall(continue(value))

          impl(manip)
        case KeepLeft(left, right: Manip[t]) =>
          given Continue[T, U] = value =>
            given Continue[t, U] = _ => tailcall(continue(value))
            tailcall(impl(right))
          impl(left)
        case KeepRight(left: Manip[t], right) =>
          given Continue[t, U] = _ =>
            given Continue[T, U] = continue
            tailcall(impl[T, U](right))
          impl(left)
        case Commit(manip, debugInfo) =>
          refMap.get(ById(Manip.tracerRef)) match
            case None =>
            case Some(ref) =>
              ref
                .asInstanceOf[Manip.Tracer]
                .onCommit(debugInfo, node)

          given Backtrack[U] = emptyBacktrack(debugInfo, node, refMap)
          impl(manip)
        case Negated(manip: Manip[t], debugInfo) =>
          given Backtrack[U] = _ => tailcall(continue(()))
          given Continue[t, U] = _ => tailcall(backtrack(debugInfo))
          impl(manip)
        case RefInit(ref, initFn, manip) =>
          given RefMap = refMap.updated(ById(ref), initFn())
          impl(manip)
        case RefGet(ref, debugInfo) =>
          refMap.get(ById(ref)) match
            case None        => tailcall(backtrack(debugInfo))
            case Some(value) => tailcall(continue(value.asInstanceOf[T]))
        case refUpdated: RefUpdated[t, T @unchecked] =>
          refMap.get(ById(refUpdated.ref)) match
            case None => tailcall(backtrack(refUpdated.debugInfo))
            case Some(value) =>
              given RefMap = refMap.updated(
                ById(refUpdated.ref),
                refUpdated.fn(value.asInstanceOf[t])
              )
              impl(refUpdated.manip)
        case ThisNode => tailcall(continue(node))
        case AtNode(manip, node) =>
          given Node.All = node
          impl(manip)
        case Disjunction(first, second) =>
          given Backtrack[U] = debugInfo1 =>
            given Backtrack[U] = debugInfo2 =>
              tailcall(backtrack(debugInfo1 ++ debugInfo2))
            tailcall(impl(second))
          impl(first)
        case Deferred(fn) => impl(fn())

    given Continue[T, T] = done
    given RefMap = Map.empty
    given Backtrack[T] = emptyBacktrack(summon[DebugInfo], node, summon[RefMap])
    given Node.All = node
    impl[T, T](this).result
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
    final def doEffect[U](using DebugInfo)(fn: T => U): Manip[U] =
      import ops.{lookahead, effect}
      get.lookahead.flatMap: v =>
        effect(fn(v))

  type Rules = Manip[Unit]

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

  object tracerRef extends Ref[Tracer]

  trait Tracer extends java.io.Closeable:
    def beforePass(debugInfo: DebugInfo, tree: Node.All): Unit
    def afterPass(debugInfo: DebugInfo, tree: Node.All): Unit
    def onCommit(debugInfo: DebugInfo, tree: Node.All): Unit
    def onFatal(debugInfo: DebugInfo, from: DebugInfo, tree: Node.All): Unit
    // def afterRewrite(debugInfo: DebugInfo, tree: Node.All): Unit

  object NopTracer extends Tracer:
    def beforePass(debugInfo: DebugInfo, tree: Node.All): Unit = ()
    def afterPass(debugInfo: DebugInfo, tree: Node.All): Unit = ()
    def onCommit(debugInfo: DebugInfo, tree: Node.All): Unit = ()
    def onFatal(debugInfo: DebugInfo, from: DebugInfo, tree: Node.All): Unit =
      ()
    def close(): Unit = ()

  final class LogTracer(out: java.io.OutputStream, limit: Int = -1)
      extends Tracer:
    export out.close

    private var lineCount: Int = 0

    private def logln(str: String): Unit =
      out.write(str.getBytes())
      out.write('\n')
      out.flush()

      lineCount += 1

      if limit != -1 && lineCount >= limit
      then throw RuntimeException(s"logged $lineCount lines")

    def beforePass(debugInfo: DebugInfo, tree: Node.All): Unit =
      logln(s"pass init $debugInfo at $tree")

    def afterPass(debugInfo: DebugInfo, tree: Node.All): Unit =
      logln(s"pass end $debugInfo at $tree")

    def onCommit(debugInfo: DebugInfo, tree: Node.All): Unit =
      logln(s"commit $debugInfo at $tree")

    def onFatal(debugInfo: DebugInfo, from: DebugInfo, tree: Node.All): Unit =
      logln(s"fatal $debugInfo from $from at $tree")

  object ops:
    def defer[T](manip: => Manip[T]): Manip[T] =
      lazy val impl = manip
      Manip.Deferred(() => impl)

    def commit[T](using DebugInfo)(manip: Manip[T]): Manip[T] =
      Manip.Commit(manip, summon[DebugInfo])

    def effect[T](fn: => T): Manip[T] =
      Manip.Effect(() => fn)

    def assertNode(using DebugInfo)(fn: Node.All => Boolean): Manip[Nothing] =
      Manip.ThisNode.lookahead.flatMap:
        case node if !fn(node) =>
          throw NodeError(
            s"assertion failed at ${summon[DebugInfo]}, looking at $node"
          )
        case _ => backtrack

    def backtrack(using DebugInfo): Manip[Nothing] =
      Manip.Backtrack(summon[DebugInfo])

    def log[T](using DebugInfo)(manip: Manip[T]): Manip[T] =
      Manip.Log(summon[DebugInfo], manip)

    def atNode[T](node: Node.All)(manip: Manip[T]): Manip[T] =
      Manip.AtNode(manip, node)

    def atRightSibling[T](using DebugInfo)(manip: Manip[T]): Manip[T] =
      Manip.ThisNode.lookahead.flatMap:
        case thisChild: Node.Child =>
          thisChild.rightSibling match
            case None => backtrack
            case Some(sibling) =>
              atNode(sibling)(manip)
        case _: Node.Root => backtrack

    def atIdxSibling[T](using DebugInfo)(idx: Int)(manip: Manip[T]): Manip[T] =
      on:
        import Pattern.ops.*
        parent(anyParent.map(_.children.lift(idx)))
      .value
        .restrict:
          case Some(child) => child
        .here(manip)

    def atFirstChild[T](using DebugInfo)(manip: Manip[T]): Manip[T] =
      Manip.ThisNode.lookahead.flatMap:
        case thisParent: Node.Parent =>
          thisParent.firstChild match
            case None => backtrack
            case Some(sibling) =>
              atNode(sibling)(manip)
        case _: Node.Embed[?] => backtrack

    def atFirstSibling[T](using DebugInfo)(manip: Manip[T]): Manip[T] =
      atParent(atFirstChild(manip))

    def atParent[T](using DebugInfo)(manip: Manip[T]): Manip[T] =
      Manip.ThisNode.lookahead.flatMap:
        case thisChild: Node.Child =>
          thisChild.parent match
            case None => backtrack
            case Some(parent) =>
              atNode(parent)(manip)
        case _: Node.Root => backtrack

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
          .product(Manip.ThisNode.filter(_.parent.nonEmpty))
          .flatMap:
            case ((maxIdx, value), node) =>
              val count =
                if maxIdx == -1 then 0
                else maxIdx - node.idxInParent + 1
              action(using
                on.Ctx(
                  count,
                  node.parent.get,
                  node.idxInParent
                )
              )(value)

    object on:
      final case class Ctx(count: Int, parent: Node.Parent, idxInParent: Int)

    def spliceThen(using onCtx: on.Ctx)(nodes: Iterable[Node.Child])(
        manip: on.Ctx ?=> Rules
    ): Rules =
      effect:
        val parent = onCtx.parent
        val idx = onCtx.idxInParent
        parent.children.patchInPlace(idx, nodes, onCtx.count)
      *> atNode(onCtx.parent):
        val rec = manip(using onCtx.copy(count = nodes.size))
        atFirstChild(atIdxSibling(onCtx.idxInParent)(rec))
          | rec

    def spliceThen(using on.Ctx)(nodes: Node.Child*)(
        manip: on.Ctx ?=> Rules
    ): Rules =
      spliceThen(nodes)(manip)

    def spliceThen(using on.Ctx)(nodes: IterableOnce[Node.Child])(
        manip: on.Ctx ?=> Rules
    ): Rules =
      spliceThen(nodes.iterator.toArray)(manip)

    def splice(using onCtx: on.Ctx, passCtx: pass.Ctx)(
        nodes: Iterable[Node.Child]
    ): Rules =
      spliceThen(nodes)(skipMatch(continuePass))

    def splice(using on.Ctx, pass.Ctx)(nodes: Node.Child*): Rules =
      splice(nodes)

    def splice(using on.Ctx, pass.Ctx)(nodes: IterableOnce[Node.Child]): Rules =
      splice(nodes.iterator.toArray)

    def continuePass(using passCtx: pass.Ctx): Rules =
      passCtx.loop

    def continuePassAtNext(using pass.Ctx): Rules =
      atNextNode(continuePass)

    def atNextNode(using passCtx: pass.Ctx)(manip: Rules): Rules =
      passCtx.strategy.atNext(manip)

    def skipMatch(using onCtx: on.Ctx)(manip: Rules): Rules =
      require(onCtx.count > 0, "tried to skip 0 nodes")
      // first rule: if we're on the parent, don't skip anything
      (ThisNode.filter(_ eq onCtx.parent) *> manip)
        | atIdxSibling(onCtx.idxInParent + onCtx.count)(manip)
        | atParent(manip)

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
      def filter(using DebugInfo)(pred: T => Boolean): Manip[T] =
        lhs.restrict:
          case v if pred(v) => v
      def withFinally(fn: => Unit): Manip[T] =
        Manip.Finally(lhs, () => fn)
      def withTracer(tracer: => Tracer): Manip[T] =
        tracerRef.init(tracer):
          tracerRef.get.lookahead.flatMap: tracer =>
            lhs.withFinally(tracer.close())

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
      require(once)
      def rules(using DebugInfo)(rules: pass.Ctx ?=> Rules): Manip[Unit] =
        val tracer = Manip.tracerRef.get | pure(Manip.NopTracer)
        val before = tracer
          .product(Manip.ThisNode)
          .flatMap: (tracer, node) =>
            effect(tracer.beforePass(summon[DebugInfo], node))
        val after = tracer
          .product(Manip.ThisNode)
          .flatMap: (tracer, node) =>
            effect(tracer.afterPass(summon[DebugInfo], node))

        lazy val loop: Manip[Unit] =
          commit:
            rules(using pass.Ctx(strategy, defer(loop)))
              | strategy.atNext(defer(loop))

        // TODO: add repeat?

        wrapFn:
          before
            *> loop
            *> after

      def withState[T](ref: Ref[T])(init: => T): pass =
        pass(
          strategy = strategy,
          once = once,
          wrapFn = manip => ref.init(init)(defer(wrapFn(manip)))
        )
    end pass

    object pass:
      final case class Ctx(strategy: TraversalStrategy, loop: Manip[Unit]):
        lazy val loopAtNext: Manip[Unit] =
          strategy.atNext(loop)

      trait TraversalStrategy:
        def atNext(manip: Manip[Unit]): Manip[Unit]

      object topDown extends TraversalStrategy:
        def atNext(manip: Manip[Unit]): Manip[Unit] =
          val next = commit(manip)
          lazy val upImpl: Manip[Unit] =
            commit:
              atRightSibling(next)
                | atParent(defer(upImpl))
                | on(Pattern.ops.theTop).check

          commit:
            atFirstChild(next)
              | upImpl

      // object bottomUp extends TraversalStrategy:
      //   def traverse(
      //       rules: Rules
      //   ): Manip[Boolean] =
      //     // TODO: fix this code, not tested and probably has similar bug to what topDown had
      //     def atBottomLeft[T](manip: Manip[T]): Manip[T] =
      //       lazy val impl: Manip[T] =
      //         atFirstChild(defer(impl))
      //           | manip

      //       impl

      //     lazy val impl: Manip[Boolean] =
      //       rules.flatMap: nextNode =>
      //         atNode(nextNode):
      //           impl.as(true)
      //       | atRightSibling:
      //         commit(defer(impl))
      //       | atParent:
      //         atRightSibling:
      //           atFirstChild:
      //             commit(defer(impl))
      //         | atFirstSibling:
      //           commit(defer(impl))
      //       | atParent(on(Pattern.ops.theTop).check.as(false))

      //     atBottomLeft(impl)
  end ops
end Manip
