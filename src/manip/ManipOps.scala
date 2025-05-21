// Copyright 2024-2025 Forja Team
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

package forja.manip

import cats.syntax.all.given

import forja.*
import forja.dsl.*
import forja.util.toShortString

import scala.reflect.ClassTag

import Manip.*

trait ManipOps:
  def instrumentWithTracer[T](tracer: Tracer)(fn: => T): T =
    require(
      tracerVar.get() eq null,
      s"tried to set over existing tracer ${tracerVar.get()}",
    )
    tracerVar.set(tracer)
    try fn
    finally
      tracer.close()
      assert(
        tracerVar.get() eq tracer,
        s"tracer $tracer was replaced by ${tracerVar.get()} during execution",
      )
      tracerVar.remove()

  def instrumentWithTracerReentrant[T, Tr <: Tracer: ClassTag](tracer: Tr)(
      fn: => T,
  ): T =
    tracerVar.get() match
      case null => instrumentWithTracer(tracer)(fn)
      case existingTracer: Tr =>
        tracerVar.remove()
        try instrumentWithTracer(tracer)(fn)
        finally tracerVar.set(existingTracer)
      case existingTracer: Tracer =>
        throw IllegalArgumentException(
          s"re-entrant tracer $tracer is not of the same type as existing tracer $existingTracer",
        )

  def defer[T](manip: => Manip[T]): Manip[T] =
    lazy val impl = manip
    Manip.Deferred(() => impl)

  def commit[T](using DebugInfo)(manip: Manip[T]): Manip[T] =
    Manip.Commit(manip, summon[DebugInfo])

  def effect[T](fn: => T): Manip[T] =
    Manip.Effect(() => fn)

  def backtrack(using DebugInfo): Manip[Nothing] =
    Manip.Backtrack(summon[DebugInfo])

  def initNode[T](using
      DebugInfo,
  )(node: Node.All)(manip: Manip[T]): Manip[T] =
    initHandle(Handle.fromNode(node))(manip)

  def initHandle[T](using
      DebugInfo,
  )(handle: Handle)(manip: Manip[T]): Manip[T] =
    Handle.ref.init(handle)(manip)

  def atNode[T](using DebugInfo)(node: Node.All)(manip: Manip[T]): Manip[T] =
    atHandle(Handle.fromNode(node))(manip)

  def atHandle[T](using
      DebugInfo,
  )(handle: Handle)(manip: Manip[T]): Manip[T] =
    restrictHandle(manip)(_ => Some(handle))

  def getHandle(using DebugInfo): Manip[Handle] =
    Handle.ref.get

  def getTracer: Manip[Tracer] =
    Manip.GetTracer

  def restrictHandle[T](using
      DebugInfo,
  )(manip: Manip[T])(fn: PartialFunction[Handle, Handle]): Manip[T] =
    Manip.RestrictHandle(fn, manip, summon[DebugInfo])

  def restrictHandle[T](using
      DebugInfo,
  )(manip: Manip[T])(fn: Handle => Option[Handle]): Manip[T] =
    Manip.RestrictHandle(fn.unlift, manip, summon[DebugInfo])

  def keepHandleIdx[T](using DebugInfo)(manip: Manip[T]): Manip[T] =
    restrictHandle(manip)(_.keepIdx)

  def keepHandlePtr[T](using DebugInfo)(manip: Manip[T]): Manip[T] =
    restrictHandle(manip)(PartialFunction.fromFunction(_.keepPtr))

  def getNode(using DebugInfo.Ctx): Manip[Node.All] =
    getHandle
      .tapEffect(_.assertCoherence())
      .restrict:
        case Handle.AtTop(top)           => top
        case Handle.AtChild(_, _, child) => child

  def atRightSibling[T](using DebugInfo)(manip: Manip[T]): Manip[T] =
    restrictHandle(manip)(_.rightSibling)

  def atLeftSibling[T](using DebugInfo)(manip: Manip[T]): Manip[T] =
    restrictHandle(manip)(_.leftSibling)

  def atIdx[T](using DebugInfo)(idx: Int)(manip: Manip[T]): Manip[T] =
    restrictHandle(manip)(_.atIdx(idx))

  def atIdxFromRight[T](using
      DebugInfo,
  )(idx: Int)(manip: Manip[T]): Manip[T] =
    restrictHandle(manip)(_.atIdxFromRight(idx))

  def atFirstChild[T](using DebugInfo)(manip: Manip[T]): Manip[T] =
    restrictHandle(manip)(_.findFirstChild)

  def atLastChild[T](using DebugInfo)(manip: Manip[T]): Manip[T] =
    restrictHandle(manip)(_.findLastChild)

  def atFirstSibling[T](using DebugInfo)(manip: Manip[T]): Manip[T] =
    atParent(atFirstChild(manip))

  def atLastSibling[T](using DebugInfo)(manip: Manip[T]): Manip[T] =
    atParent(atLastChild(manip))

  def atParent[T](using DebugInfo)(manip: Manip[T]): Manip[T] =
    restrictHandle(manip)(_.findParent)

  def atAncestor[T](using DebugInfo.Ctx)(manip: Manip[T]): Manip[T] =
    lazy val impl: Manip[T] =
      manip | atParent(defer(impl))

    atParent(impl)

  def addChild[T](using
      DebugInfo.Ctx,
  )(child: => Node.Child): Manip[Node.Child] =
    getNode.lookahead.flatMap:
      case thisParent: Node.Parent =>
        effect:
          val tmp = child
          thisParent.children.addOne(tmp)
          tmp
      case _ => backtrack

  final class on[T](val pattern: SeqPattern[T]):
    def raw: Manip[SeqPattern.Result[T]] =
      pattern.manip

    def value: Manip[T] =
      raw.map(_.value)

    def check: Manip[Unit] =
      raw.void

    def rewrite(using
        DebugInfo.Ctx,
    )(
        action: on.Ctx ?=> T => Rules,
    ): Rules =
      (raw, getHandle, getTracer).tupled
        .flatMap: (patResult, handle, tracer) =>
          handle.assertCoherence()
          val value = patResult.value
          val (parent, startIdx) =
            handle match
              case Handle.AtTop(top) =>
                throw NodeError(
                  s"tried to rewrite top ${top.toShortString()}",
                )
              case Handle.AtChild(parent, idx, _) => (parent, idx)
              case Handle.Sentinel(parent, idx)   => (parent, idx)
          val matchedCount =
            patResult match
              case SeqPattern.Result.Top(top, _) =>
                throw NodeError(
                  s"ended pattern at top ${top.toShortString()}",
                )
              case patResult: (SeqPattern.Result.Look[T] |
                    SeqPattern.Result.Match[T]) =>
                assert(patResult.parent eq parent)
                if patResult.isMatch
                then patResult.idx - startIdx + 1
                else patResult.idx - startIdx

          assert(
            matchedCount >= 1,
            "can't rewrite based on a pattern that matched nothing",
          )
          tracer.onRewriteMatch(
            summon[DebugInfo],
            parent,
            startIdx,
            matchedCount,
          )
          action(using on.Ctx(matchedCount))(value)

  object on:
    final case class Ctx(matchedCount: Int)

  def spliceThen(using
      DebugInfo.Ctx,
  )(using
      onCtx: on.Ctx,
  )(
      nodes: Iterable[Node.Child],
  )(
      manip: on.Ctx ?=> Rules,
  ): Rules =
    /* Preparation for the rewrite may have reparented the node we were "on"
     * When you immediately call splice, this pretty much always means "stay at
     * that index and put something there", so that's what we do.
     * Much easier than forcing the end user to understand such a confusing edge
     * case. */
    keepHandleIdx:
      (getHandle, getTracer).tupled
        .tapEffect: (handle, tracer) =>
          handle.assertCoherence()
          val (parent, idx) =
            handle match
              case Handle.AtTop(top) =>
                throw NodeError(s"tried to splice top ${top.toShortString()}")
              case Handle.AtChild(parent, idx, _) => (parent, idx)
              case Handle.Sentinel(parent, idx)   => (parent, idx)
          parent.children.patchInPlace(idx, nodes, onCtx.matchedCount)
          tracer.onRewriteComplete(
            summon[DebugInfo],
            parent,
            idx,
            nodes.size,
          )
      *> keepHandleIdx(
        pass.resultRef.updated(_ => RulesResult.Progress)(
          manip(using on.Ctx(nodes.size)),
        ),
      )

  def spliceThen(using
      DebugInfo,
      on.Ctx,
  )(nodes: Node.Child*)(
      manip: on.Ctx ?=> Rules,
  ): Rules =
    spliceThen(nodes)(manip)

  def spliceThen(using
      DebugInfo,
      on.Ctx,
  )(nodes: IterableOnce[Node.Child])(
      manip: on.Ctx ?=> Rules,
  ): Rules =
    spliceThen(nodes.iterator.toArray)(manip)

  def splice(using
      DebugInfo.Ctx,
  )(using
      onCtx: on.Ctx,
      passCtx: pass.Ctx,
  )(
      nodes: Iterable[Node.Child],
  ): Rules =
    spliceThen(nodes):
      /* If we _now_ have a match count of 0, it means the splice deleted our
       * match. In that case, it's safe to retry at same position because we
       * changed something. Normally that's bad, because it means we matched
       * nothing and will retry in same position. */
      if summon[on.Ctx].matchedCount == 0
      then continuePass
      else skipMatch

  def splice(using DebugInfo, on.Ctx, pass.Ctx)(nodes: Node.Child*): Rules =
    splice(nodes)

  def splice(using
      DebugInfo,
      on.Ctx,
      pass.Ctx,
  )(
      nodes: IterableOnce[Node.Child],
  ): Rules =
    splice(nodes.iterator.toArray)

  def continuePass(using passCtx: pass.Ctx): Rules =
    passCtx.loop

  def continuePassAtNextNode(using DebugInfo)(using pass.Ctx): Rules =
    atNextNode(continuePass)

  def atNextNode(using
      DebugInfo,
  )(using passCtx: pass.Ctx)(manip: Rules): Rules =
    passCtx.strategy.atNext(manip)

  def endPass(using DebugInfo): Rules =
    pass.resultRef.get

  def skipMatch(using
      DebugInfo.Ctx,
  )(using onCtx: on.Ctx, passCtx: pass.Ctx): Rules =
    require(
      onCtx.matchedCount > 0,
      s"must have matched at least one node to skip. Matched ${onCtx.matchedCount}",
    )
    getHandle.lookahead.flatMap: handle =>
      handle.assertCoherence()
      val idxOpt =
        handle match
          case Handle.AtTop(top)         => None
          case Handle.AtChild(_, idx, _) => Some(idx)
          case Handle.Sentinel(_, idx)   => Some(idx)

      idxOpt match
        case None => endPass
        case Some(idx) =>
          atIdx(idx + onCtx.matchedCount)(continuePass)

  extension [T](lhs: Manip[T])
    @scala.annotation.targetName("or")
    def |(using DebugInfo)(rhs: Manip[T]): Manip[T] =
      Manip.monoidK.combineK(lhs, rhs)
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
    def tapEffect(fn: T => Unit): Manip[T] =
      Manip.TapEffect(lhs, fn)

  extension (lhs: Manip[Node.All])
    def here[U](using DebugInfo.Ctx)(manip: Manip[U]): Manip[U] =
      lhs.lookahead.flatMap: node =>
        restrictHandle(manip)(_ => Some(Handle.fromNode(node)))

  final class pass(
      strategy: pass.TraversalStrategy = pass.topDown,
      once: Boolean = false,
      wrapFn: Manip[Unit] => Manip[Unit] = identity,
  ):
    def rules(using DebugInfo.Ctx)(rules: pass.Ctx ?=> Rules): Manip[Unit] =
      val before = getTracer
        .flatMap: tracer =>
          effect(tracer.beforePass(summon[DebugInfo]))
      val after = getTracer
        .flatMap: tracer =>
          effect(tracer.afterPass(summon[DebugInfo]))

      // TODO: consider optimizing the ancestor check if it becomes a bottleneck
      def exceptInError[T](manip: Manip[T])(using DebugInfo.Ctx): Manip[T] =
        on(not(ancestor(Builtin.Error))).check *> manip

      lazy val loop: Manip[RulesResult] =
        commit:
          exceptInError(rules(using pass.Ctx(strategy, defer(loop))))
            | strategy.atNext(defer(loop))

      wrapFn:
        lazy val bigLoop: Manip[Unit] =
          pass.resultRef.reset:
            pass.resultRef.init(RulesResult.NoProgress):
              (before *> strategy.atInit(loop) <* after).flatMap:
                case RulesResult.Progress if !once =>
                  bigLoop
                case RulesResult.Progress | RulesResult.NoProgress =>
                  pure(())

        bigLoop

    def withState[T](ref: Ref[T])(init: => T): pass =
      pass(
        strategy = strategy,
        once = once,
        wrapFn = manip => ref.init(init)(defer(wrapFn(manip))),
      )
  end pass

  object pass:
    object resultRef extends Ref[RulesResult]

    final case class Ctx(
        strategy: TraversalStrategy,
        loop: Manip[RulesResult],
    )(using DebugInfo.Ctx):
      lazy val loopAtNext: Manip[RulesResult] =
        strategy.atNext(loop)

    trait TraversalStrategy:
      def atInit(manip: Manip[RulesResult])(using
          DebugInfo.Ctx,
      ): Manip[RulesResult]
      def atNext(manip: Manip[RulesResult])(using
          DebugInfo.Ctx,
      ): Manip[RulesResult]

    object topDown extends TraversalStrategy:
      def atInit(manip: Manip[RulesResult])(using
          DebugInfo.Ctx,
      ): Manip[RulesResult] = manip

      def atNext(manip: Manip[RulesResult])(using
          DebugInfo.Ctx,
      ): Manip[RulesResult] =
        val next = commit(manip)
        commit:
          atFirstChild(next)
            | atRightSibling(next)
            | (on(atEnd).check
              *> atParent(
                commit(
                  /* going right finds either real sibling or sentinel, unless
                   * at top */
                  atRightSibling(next)
                    | on(theTop).check *> endPass,
                ),
              ))

    object bottomUp extends TraversalStrategy:
      def atInit(manip: Manip[RulesResult])(using
          DebugInfo.Ctx,
      ): Manip[RulesResult] =
        lazy val impl: Manip[RulesResult] =
          commit:
            atFirstChild(defer(impl))
              | manip

        impl

      def atNext(manip: Manip[RulesResult])(using
          DebugInfo.Ctx,
      ): Manip[RulesResult] =
        val next = commit(manip)
        def atNextCousin[T](manip: Manip[T]): Manip[T] =
          lazy val impl: Manip[T] =
            atFirstChild(manip)
              | atRightSibling(defer(impl))

          atParent(atRightSibling(impl))

        commit:
          /* same layer, same parent (next case ensures we already processed any
           * children) */
          atRightSibling(next)
          // go all the way into next subtree on the right
            | atNextCousin(atInit(next))
            /* up one layer, far left, knowing we looked at all reachable
             * children */
            | atParent(atFirstSibling(next))
            // special case: parent is top
            | atParent(on(theTop).check *> next)
            // onward from top --> done
            | on(theTop).check *> endPass
end ManipOps
