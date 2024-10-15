package distcompiler

import cats.syntax.all.given

enum Manip[+T]:
  private inline given DebugInfo = DebugInfo.poison

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
  case GetRefMap extends Manip[Manip.RefMap]
  case GetTracer extends Manip[Manip.Tracer]

  case Disjunction(first: Manip[T], second: Manip[T])
  case Deferred(fn: () => Manip[T])

  def isBacktrack: Boolean =
    this match
      case Backtrack(_) => true
      case _            => false

  def perform(using DebugInfo)(): T =
    import scala.util.control.TailCalls.*

    type Continue[-T, +U] = T => TailRec[U]
    type Backtrack[+U] = DebugInfo => TailRec[U]
    type RefMap = Manip.RefMap

    def tracer(using refMap: RefMap): Manip.Tracer =
      refMap.get(Manip.tracerRef) match
        case None         => Manip.NopTracer
        case Some(tracer) => tracer

    def handleOpt(using refMap: RefMap): Option[Manip.Handle] =
      refMap.get(Manip.Handle.ref)

    def emptyBacktrack(
        ctxInfo: DebugInfo,
        refMap: RefMap
    ): Backtrack[Nothing] =
      posInfo =>
        tracer(using refMap).onFatal(ctxInfo, posInfo)(using refMap)

        throw RuntimeException(
          s"unrecovered backtrack at $posInfo, caught at $ctxInfo, at ${refMap.treeDescr}"
        )

    def impl[T, U](self: Manip[T])(using
        continue: Continue[T, U],
        backtrack: Backtrack[U],
        refMap: RefMap
    ): TailRec[U] =
      self match
        case Backtrack(debugInfo) =>
          tracer.onBacktrack(debugInfo)
          tailcall(backtrack(debugInfo))
        case Pure(value) => tailcall(continue(value))
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
                tracer.onBacktrack(restrict.debugInfo)
                tailcall(backtrack(restrict.debugInfo))
              case Some(value) =>
                tailcall(continue(value))
          impl(restrict.manip)
        case Effect(fn) =>
          val value = fn()
          tailcall(continue(value))
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
          tracer.onCommit(debugInfo)
          given Backtrack[U] = emptyBacktrack(debugInfo, refMap)
          impl(manip)
        case Negated(manip: Manip[t], debugInfo) =>
          given Backtrack[U] = _ => tailcall(continue(()))
          given Continue[t, U] = _ =>
            tracer.onBacktrack(debugInfo)
            tailcall(backtrack(debugInfo))
          impl(manip)
        case RefInit(ref, initFn, manip) =>
          given RefMap = refMap.updated(ref, initFn())
          impl(manip)
        case RefGet(ref, debugInfo) =>
          refMap.get(ref) match
            case None =>
              tracer.onBacktrack(debugInfo)
              tailcall(backtrack(debugInfo))
            case Some(value) => tailcall(continue(value))
        case refUpdated: RefUpdated[t, T @unchecked] =>
          refMap.get(refUpdated.ref) match
            case None =>
              tracer.onBacktrack(refUpdated.debugInfo)
              tailcall(backtrack(refUpdated.debugInfo))
            case Some(value) =>
              given RefMap = refMap.updated(
                refUpdated.ref,
                refUpdated.fn(value)
              )
              impl(refUpdated.manip)
        case GetRefMap =>
          tailcall(continue(refMap))
        case GetTracer =>
          tailcall(continue(tracer))
        case Disjunction(first, second) =>
          given Backtrack[U] = debugInfo1 =>
            given Backtrack[U] = debugInfo2 =>
              tailcall(backtrack(debugInfo1 ++ debugInfo2))
            tailcall(impl(second))
          impl(first)
        case Deferred(fn) => impl(fn())

    given Continue[T, T] = done
    given RefMap = Manip.RefMap.empty
    given Backtrack[T] = emptyBacktrack(summon[DebugInfo], summon[RefMap])
    impl[T, T](this).result
  end perform

  // TODO: optimize disjunctions to decision tries
end Manip

object Manip:
  private inline given DebugInfo = DebugInfo.poison

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

  final class RefMap(private val map: Map[ById[Manip.Ref[?]], Any])
      extends AnyVal:
    def get[T](ref: Ref[T]): Option[T] =
      map.get(ById(ref)).asInstanceOf[Option[T]]

    def updated[T](ref: Ref[T], value: T): RefMap =
      RefMap(map.updated(ById(ref), value))

    def treeDescr: String =
      get(Handle.ref) match
        case None => "<no tree>"
        case Some(handle) =>
          handle match
            case Handle.AtTop(top) => s"top $top"
            case Handle.AtChild(parent, idx, child) =>
              if parent.children.lift(idx).exists(_ eq child)
              then s"child $idx of $parent"
              else s"!!mismatch child $idx of $parent\n!!and $child"
            case Handle.Sentinel(parent, idx) =>
              s"end [idx=$idx] of $parent"

  object RefMap:
    def empty: RefMap = RefMap(Map.empty)

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

  // given monoidK(using DebugInfo): cats.MonoidK[Manip] with
  //   export semigroupK.combineK
  //   def empty[A]: Manip[A] = Manip.Backtrack(summon[DebugInfo])

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
    def beforePass(debugInfo: DebugInfo)(using RefMap): Unit
    def afterPass(debugInfo: DebugInfo)(using RefMap): Unit
    def onCommit(debugInfo: DebugInfo)(using RefMap): Unit
    def onBacktrack(debugInfo: DebugInfo)(using RefMap): Unit
    def onFatal(debugInfo: DebugInfo, from: DebugInfo)(using RefMap): Unit
    def onRewriteMatch(
        debugInfo: DebugInfo,
        parent: Node.Parent,
        idx: Int,
        matchedCount: Int
    )(using RefMap): Unit
    def onRewriteComplete(
        debugInfo: DebugInfo,
        parent: Node.Parent,
        idx: Int,
        resultCount: Int
    )(using RefMap): Unit

  object NopTracer extends Tracer:
    def beforePass(debugInfo: DebugInfo)(using RefMap): Unit = ()
    def afterPass(debugInfo: DebugInfo)(using RefMap): Unit = ()
    def onCommit(debugInfo: DebugInfo)(using RefMap): Unit = ()
    def onBacktrack(debugInfo: DebugInfo)(using RefMap): Unit = ()
    def onFatal(debugInfo: DebugInfo, from: DebugInfo)(using RefMap): Unit = ()
    def onRewriteMatch(
        debugInfo: DebugInfo,
        parent: Node.Parent,
        idx: Int,
        matchedCount: Int
    )(using RefMap): Unit = ()
    def onRewriteComplete(
        debugInfo: DebugInfo,
        parent: Node.Parent,
        idx: Int,
        resultCount: Int
    )(using RefMap): Unit = ()
    def close(): Unit = ()

  final class LogTracer(out: java.io.OutputStream, limit: Int = -1)
      extends Tracer:
    def this(path: os.Path, limit: Int) =
      this(os.write.over.outputStream(path, createFolders = true), limit)
    def this(path: os.Path) =
      this(path, limit = -1)

    export out.close

    private var lineCount: Int = 0

    private def logln(str: String): Unit =
      out.write(str.getBytes())
      out.write('\n')
      out.flush()

      lineCount += 1

      if limit != -1 && lineCount >= limit
      then throw RuntimeException(s"logged $lineCount lines")

    private def treeDesc(using RefMap): String =
      summon[RefMap].treeDescr

    def beforePass(debugInfo: DebugInfo)(using RefMap): Unit =
      logln(s"pass init $debugInfo at $treeDesc")

    def afterPass(debugInfo: DebugInfo)(using RefMap): Unit =
      logln(s"pass end $debugInfo at $treeDesc")

    def onCommit(debugInfo: DebugInfo)(using RefMap): Unit =
      logln(s"commit $debugInfo at $treeDesc")

    def onBacktrack(debugInfo: DebugInfo)(using RefMap): Unit =
      logln(s"backtrack $debugInfo at $treeDesc")

    def onFatal(debugInfo: DebugInfo, from: DebugInfo)(using RefMap): Unit =
      logln(s"fatal $debugInfo from $from at $treeDesc")

    def onRewriteMatch(
        debugInfo: DebugInfo,
        parent: Node.Parent,
        idx: Int,
        matchedCount: Int
    )(using RefMap): Unit =
      logln(
        s"rw match $debugInfo, from $idx, $matchedCount nodes in parent $parent"
      )

    def onRewriteComplete(
        debugInfo: DebugInfo,
        parent: Node.Parent,
        idx: Int,
        resultCount: Int
    )(using RefMap): Unit =
      logln(
        s"rw done $debugInfo, from $idx, $resultCount nodes in parent $parent"
      )

  enum Handle:
    assertCoherence()

    case AtTop(top: Node.Top)
    case AtChild(parent: Node.Parent, idx: Int, child: Node.Child)
    case Sentinel(parent: Node.Parent, idx: Int)

    def assertCoherence(): Unit =
      this match
        case AtTop(_) =>
        case AtChild(parent, idx, child) =>
          if parent.children.isDefinedAt(idx) && (parent.children(idx) eq child)
          then () // ok
          else
            throw NodeError(s"mismatch: idx $idx in $parent and ptr -> $child")
        case Sentinel(parent, idx) =>
          if parent.children.length != idx
          then
            throw NodeError(
              s"mismatch: sentinel at $idx does not point to end ${parent.children.length} of $parent"
            )

    def rightSibling: Option[Handle] =
      assertCoherence()
      this match
        case AtTop(_) => None
        case AtChild(parent, idx, _) =>
          Handle.idxIntoParent(parent, idx + 1)
        case Sentinel(_, _) => None

    def findFirstChild: Option[Handle] =
      assertCoherence()
      this match
        case AtTop(top) => Handle.idxIntoParent(top, 0)
        case AtChild(_, _, child) =>
          child match
            case child: Node.Parent => Handle.idxIntoParent(child, 0)
            case _: Node.Embed[?]   => None
        case Sentinel(_, _) => None

    def findParent: Option[Handle] =
      assertCoherence()
      this match
        case AtTop(_)              => None
        case AtChild(parent, _, _) => Some(Handle.fromNode(parent))
        case Sentinel(parent, _)   => Some(Handle.fromNode(parent))

    def atIdx(idx: Int): Option[Handle] =
      assertCoherence()
      this match
        case AtTop(_)              => None
        case AtChild(parent, _, _) => Handle.idxIntoParent(parent, idx)
        case Sentinel(parent, _)   => Handle.idxIntoParent(parent, idx)

    def keepIdx: Option[Handle] =
      this match
        case AtTop(_)                => Some(this)
        case AtChild(parent, idx, _) => Handle.idxIntoParent(parent, idx)
        case Sentinel(parent, idx)   => Handle.idxIntoParent(parent, idx)

    def keepPtr: Handle =
      this match
        case AtTop(_)             => this
        case AtChild(_, _, child) => Handle.fromNode(child)
        case Sentinel(parent, _) =>
          Handle.idxIntoParent(parent, parent.children.length).get

  object Handle:
    object ref extends Ref[Handle]

    def fromNode(node: Node.All): Handle =
      node match
        case top: Node.Top => Handle.AtTop(top)
        case child: Node.Child =>
          require(child.parent.nonEmpty)
          Handle.AtChild(child.parent.get, child.idxInParent, child)

    private def idxIntoParent(parent: Node.Parent, idx: Int): Option[Handle] =
      if parent.children.isDefinedAt(idx)
      then Some(AtChild(parent, idx, parent.children(idx)))
      else if idx == parent.children.length
      then Some(Sentinel(parent, idx))
      else None

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

    def atNode[T](node: Node.All)(manip: Manip[T]): Manip[T] =
      atHandle(Handle.fromNode(node))(manip)

    def atHandle[T](handle: Manip.Handle)(manip: Manip[T]): Manip[T] =
      Manip.Handle.ref.init(handle)(manip)

    def getHandle(using DebugInfo): Manip[Manip.Handle] =
      Manip.Handle.ref.get

    def getTracer: Manip[Manip.Tracer] =
      Manip.GetTracer

    def keepHandleIdx[T](using DebugInfo)(manip: Manip[T]): Manip[T] =
      getHandle.lookahead.flatMap: handle =>
        handle.keepIdx match
          case None         => backtrack
          case Some(handle) => Manip.Handle.ref.updated(_ => handle)(manip)

    def keepHandlePtr[T](using DebugInfo)(manip: Manip[T]): Manip[T] =
      getHandle.lookahead.flatMap: handle =>
        Manip.Handle.ref.updated(_ => handle.keepPtr)(manip)

    def getNode(using DebugInfo): Manip[Node.All] =
      getHandle
        .tapEffect(_.assertCoherence())
        .restrict:
          case Handle.AtTop(top)           => top
          case Handle.AtChild(_, _, child) => child

    def atRightSibling[T](using DebugInfo)(manip: Manip[T]): Manip[T] =
      getHandle.lookahead.flatMap: handle =>
        handle.rightSibling match
          case None => backtrack
          case Some(handle) =>
            Manip.Handle.ref.updated(_ => handle)(manip)

    def atIdx[T](using DebugInfo)(idx: Int)(manip: Manip[T]): Manip[T] =
      getHandle.lookahead.flatMap: handle =>
        handle.atIdx(idx) match
          case None         => backtrack
          case Some(handle) => Manip.Handle.ref.updated(_ => handle)(manip)

    def atFirstChild[T](using DebugInfo)(manip: Manip[T]): Manip[T] =
      getHandle.lookahead.flatMap: handle =>
        handle.findFirstChild match
          case None         => backtrack
          case Some(handle) => Manip.Handle.ref.updated(_ => handle)(manip)

    def atFirstSibling[T](using DebugInfo)(manip: Manip[T]): Manip[T] =
      atParent(atFirstChild(manip))

    def atParent[T](using DebugInfo)(manip: Manip[T]): Manip[T] =
      getHandle.lookahead.flatMap: handle =>
        handle.findParent match
          case None         => backtrack
          case Some(handle) => Manip.Handle.ref.updated(_ => handle)(manip)

    def addChild[T](using DebugInfo)(child: => Node.Child): Manip[Node.Child] =
      getNode.lookahead.flatMap:
        case thisParent: Node.Parent =>
          effect:
            val tmp = child
            thisParent.children.addOne(tmp)
            tmp
        case _ => backtrack

    final class on[T](val pattern: Pattern[T]):
      def raw: Manip[(Int, T)] =
        pattern.manip

      def value: Manip[T] =
        raw.map(_._2)

      def check: Manip[Unit] =
        raw.as(())

      def rewrite(using DebugInfo)(
          action: on.Ctx ?=> T => Rules
      ): Rules =
        (raw, getHandle, getTracer, Manip.GetRefMap).tupled
          .flatMap:
            case ((maxIdx, value), handle, tracer, refMap) =>
              handle.assertCoherence()
              val (parent, idx) =
                handle match
                  case Handle.AtTop(top) =>
                    throw NodeError(s"tried to rewrite top $top")
                  case Handle.AtChild(parent, idx, _) => (parent, idx)
                  case Handle.Sentinel(parent, idx)   => (parent, idx)
              val matchedCount =
                if maxIdx == -1
                then 0
                else
                  maxIdx + 1 - idx // +1 because if we matched range 2 - 2 then we saw 1 node
              tracer.onRewriteMatch(
                summon[DebugInfo],
                parent,
                idx,
                matchedCount
              )(using refMap)
              action(using on.Ctx(matchedCount))(value)

    object on:
      final case class Ctx(matchedCount: Int) extends AnyVal

    def spliceThen(using DebugInfo)(using onCtx: on.Ctx)(
        nodes: Iterable[Node.Child]
    )(
        manip: on.Ctx ?=> Rules
    ): Rules =
      (getHandle, getTracer, Manip.GetRefMap).tupled
        .tapEffect: (handle, tracer, refMap) =>
          handle.assertCoherence()
          val (parent, idx) =
            handle match
              case Handle.AtTop(top) =>
                throw NodeError(s"tried to splice top $top")
              case Handle.AtChild(parent, idx, _) => (parent, idx)
              case Handle.Sentinel(parent, idx)   => (parent, idx)
          parent.children.patchInPlace(idx, nodes, onCtx.matchedCount)
          tracer.onRewriteComplete(summon[DebugInfo], parent, idx, nodes.size)(
            using refMap
          )
      *> keepHandleIdx(manip(using on.Ctx(nodes.size)))

    def spliceThen(using DebugInfo, on.Ctx)(nodes: Node.Child*)(
        manip: on.Ctx ?=> Rules
    ): Rules =
      spliceThen(nodes)(manip)

    def spliceThen(using DebugInfo, on.Ctx)(nodes: IterableOnce[Node.Child])(
        manip: on.Ctx ?=> Rules
    ): Rules =
      spliceThen(nodes.iterator.toArray)(manip)

    def splice(using DebugInfo)(using onCtx: on.Ctx, passCtx: pass.Ctx)(
        nodes: Iterable[Node.Child]
    ): Rules =
      spliceThen(nodes)(skipMatch)

    def splice(using DebugInfo, on.Ctx, pass.Ctx)(nodes: Node.Child*): Rules =
      splice(nodes)

    def splice(using DebugInfo, on.Ctx, pass.Ctx)(
        nodes: IterableOnce[Node.Child]
    ): Rules =
      splice(nodes.iterator.toArray)

    def continuePass(using passCtx: pass.Ctx): Rules =
      passCtx.loop

    def continuePassAtNextNode(using pass.Ctx): Rules =
      atNextNode(continuePass)

    def atNextNode(using passCtx: pass.Ctx)(manip: Rules): Rules =
      passCtx.strategy.atNext(manip)

    def skipMatch(using
        DebugInfo
    )(using onCtx: on.Ctx, passCtx: pass.Ctx): Rules =
      getHandle.lookahead.flatMap: handle =>
        handle.assertCoherence()
        val idx =
          handle match
            case Handle.AtTop(top) =>
              throw NodeError(s"tried to skip match at top $top")
            case Handle.AtChild(_, idx, _) => idx
            case Handle.Sentinel(_, idx)   => idx

        atIdx(idx + (onCtx.matchedCount - 1).max(0))(continuePassAtNextNode)

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
      def withTracer(using DebugInfo)(tracer: => Tracer): Manip[T] =
        tracerRef.init(tracer):
          getTracer.lookahead.flatMap: tracer =>
            lhs.withFinally(tracer.close())
      def tapEffect(fn: T => Unit): Manip[T] =
        lookahead.flatMap: value =>
          effect:
            fn(value)
            value

    extension (lhs: Manip[Node.All])
      def here[U](manip: Manip[U]): Manip[U] =
        lhs.lookahead.flatMap: node =>
          atNode(node)(manip)

    final class pass(
        strategy: pass.TraversalStrategy = pass.topDown,
        once: Boolean = false,
        wrapFn: Manip[Unit] => Manip[Unit] = identity
    ):
      require(once)
      def rules(using DebugInfo)(rules: pass.Ctx ?=> Rules): Manip[Unit] =
        val before = getTracer
          .product(Manip.GetRefMap)
          .flatMap: (tracer, refMap) =>
            effect(tracer.beforePass(summon[DebugInfo])(using refMap))
        val after = getTracer
          .product(Manip.GetRefMap)
          .flatMap: (tracer, refMap) =>
            effect(tracer.afterPass(summon[DebugInfo])(using refMap))

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
        private inline given DebugInfo = DebugInfo.notPoison

        def atNext(manip: Manip[Unit]): Manip[Unit] =
          val next = commit(manip)
          commit:
            atFirstChild(next)
              | atRightSibling(next)
              | on(Pattern.ops.atEnd).check
              *> atParent:
                commit:
                  atRightSibling(next)
                    | on(Pattern.ops.theTop).check

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
