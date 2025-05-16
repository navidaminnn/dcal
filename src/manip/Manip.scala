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
import forja.util.SymbolicMapFactory
import scala.util.NotGiven
import scala.reflect.ClassTag

/** The core DSL class, whose [[forja.manip.Manip.perform]] method causes the
  * represented instructions to run. It has instances of [[cats]] typeclasses,
  * and therefore inherits some subset of syntax from that library:
  *   - [[forja.manip.Manip.applicative]] allows applicative functor
  *     composition, such as
  *     - `Manip.pure(41).map(_ + 1)` mapping the result of one op
  *     - Keep right `Manip.unit *> Manip.pure("keep this side")``
  *     - Keep left `Manip.pure("keep this side") <* Manip.unit`
  *     - Tuple of ops into one op that gives tuple of results
  *       `(Manip.pure("elem 1"), Manip.pure("elem 2")).tupled`
  *     - `(Manip.pure(2), Manip.pure(2)).mapN(_ + _)` similar to above, but
  *       directly applying a function to results
  *   - [[forja.manip.Manip.monoidK]] enables cats utilities that rely on
  *     "choice" and "empty" operations
  *
  * The specific subclasses are an implementation detail. See
  * [[forja.dsl]] for instructions on how to construct instances of this
  * class.
  */
sealed abstract class Manip[+T]:
  private inline given DebugInfo = DebugInfo.poison

  private[forja] def isBacktrack: Boolean = false

  private[manip] def performImpl(state: Manip.PerformState): Manip.PerformResult

  final def perform(using DebugInfo)(): T =
    def impl: T =
      val state = Manip.PerformState(summon[DebugInfo])

      var performResult: Manip.PerformResult = this
      while performResult ne null do
        assert(
          state.result == null,
          s"result ${state.result} should have been null"
        )
        performResult = performResult.performImpl(state)
        if performResult eq null
        then performResult = state.popNextResult()
      end while

      state.result.asInstanceOf[T]
    end impl

    if Manip.useReferenceTracer
    then instrumentWithTracerReentrant(ReferenceTracer(this))(impl)
    else impl
end Manip

object Manip:
  private inline given poison(using NotGiven[DebugInfo.Ctx]): DebugInfo =
    DebugInfo.poison

  final case class UnrecoveredBacktrackException(
      rootInfo: DebugInfo,
      debugInfo: DebugInfo
  ) extends RuntimeException(
        s"unrecovered backtrack caught in ($rootInfo), initiated at $debugInfo"
      )

  private val useReferenceTracer =
    val propName = "distcompiler.Manip.useReferenceTracer"
    System.getProperty(propName) match
      case null  => false
      case "yes" => true
      case "no"  => false
      case idk =>
        throw RuntimeException(s"invalid property value \"$idk\" for $propName")
  private[manip] var tracerVar: ThreadLocal[Tracer] = ThreadLocal()

  private[manip] final class PerformState(debugInfo: DebugInfo):
    var result: Any | Null = null
    val refMap = RefMap.empty
    refMap(PerformState.RootDebugInfoRef) = debugInfo
    val committedStack = SegmentedStack[PerformState.StackRec]()
    val speculateStack = SegmentedStack[PerformState.StackRec]()

    def rootDebugInfo: DebugInfo =
      refMap(PerformState.RootDebugInfoRef)

    private val safeMap = RefMapFactory.Map.empty[Int](-1)
    private var leftDepth = 0
    private var committedDepth = 0
    def startCommit(debugInfo: DebugInfo): Unit =
      pushSave(PerformState.RootDebugInfoRef, rootDebugInfo)
      refMap(PerformState.RootDebugInfoRef) = debugInfo
      if committedDepth <= leftDepth
      then leftDepth = committedDepth
    def finishCommit(): Unit =
      committedDepth = leftDepth
    def leftInc(): Unit =
      leftDepth += 1
    def leftDec(): Unit =
      leftDepth -= 1
      assert(leftDepth >= 0)
    def pushSave[T](
        ref: Ref[T],
        value: T | Null,
        oldSavedDepth: Int = -2,
        isCommit: Boolean = false
    ): Unit =
      // if we have an old depth that "predates" what's in safeMap,
      // restore to that. This will catch the first RestoreRef during commit, if
      // the safeMap entry has advanced since then.
      // There's a theorem here about how we cannot speculate with leftDepth less than
      // what we've committed.
      if oldSavedDepth != -2 && oldSavedDepth < safeMap(ref)
      then safeMap(ref) = oldSavedDepth

      val savedDepth = safeMap(ref)
      assert(savedDepth <= leftDepth)
      if savedDepth < leftDepth
      then
        val rec = PerformState.RestoreRef(ref, value, savedDepth)
        if isCommit
        then committedStack.push(rec)
        else speculateStack.push(rec)
        safeMap(ref) = leftDepth
    def pushSaveDel[T](ref: Ref[T]): Unit =
      pushSave(ref, null)

    val tracer: Tracer = tracerVar.get() match
      case null   => NopTracer
      case tracer => tracer

    def backtrack(self: Manip[?], posInfo: DebugInfo): PerformResult =
      tracer.onBacktrack(self, posInfo)
      result = null.asInstanceOf
      var backtrackResult: PerformResult = null
      while backtrackResult eq null do
        speculateStack.pop() match
          case null =>
            throw UnrecoveredBacktrackException(rootDebugInfo, posInfo)
          case rec => backtrackResult = rec.performBacktrack(this, posInfo)
      backtrackResult

    def popNextResult(): PerformResult =
      var popResult: PerformResult = null
      while popResult eq null do
        var popped = speculateStack.pop()
        if popped `eq` null
        then popped = committedStack.pop()

        popped match
          case null => return null
          case elem =>
            popResult = elem.performPop(this)
      popResult
  end PerformState

  private[manip] object PerformState:
    private object RootDebugInfoRef extends Manip.Ref[DebugInfo]

    sealed trait StackRec:
      private[manip] def performPop(
          state: Manip.PerformState
      ): Manip.PerformResult
      private[manip] def performBacktrack(
          state: Manip.PerformState,
          posInfo: DebugInfo
      ): Manip.PerformResult
      private[manip] def performCommit(state: Manip.PerformState): Unit

    sealed trait NopBacktrack extends StackRec:
      def performBacktrack(
          state: PerformState,
          posInfo: DebugInfo
      ): PerformResult = null

    sealed trait PopPop extends StackRec:
      def performPop(
          state: Manip.PerformState
      ): Manip.PerformResult = null

    sealed trait PushCommit extends StackRec:
      private[manip] def performCommit(state: PerformState): Unit =
        state.committedStack.push(this)

    final case class RestoreRef[T](ref: Ref[T], value: T | Null, leftDepth: Int)
        extends StackRec:
      private def restore(state: PerformState): Unit =
        state.safeMap(ref) = leftDepth
        if value == null
        then state.refMap.remove(ref)
        else state.refMap(ref) = value.nn
      def performBacktrack(
          state: PerformState,
          posInfo: DebugInfo
      ): PerformResult =
        restore(state)
        null
      def performPop(state: PerformState): PerformResult =
        restore(state)
        null
      override def performCommit(state: PerformState): Unit =
        state.pushSave(ref, value, oldSavedDepth = leftDepth, isCommit = true)

    final class MapFn[T, U](fn: T => U) extends NopBacktrack, PushCommit:
      def performPop(state: PerformState): PerformResult =
        state.result = fn(state.result.asInstanceOf[T])
        null

  import PerformState.*

  private[manip] type PerformResult = Manip[?] | Null

  final case class Backtrack(debugInfo: DebugInfo) extends Manip[Nothing]:
    private[forja] override def isBacktrack: Boolean = true
    private[manip] def performImpl(state: PerformState): PerformResult =
      state.backtrack(this, debugInfo)

  final case class Pure[T](value: T) extends Manip[T]:
    def performImpl(state: PerformState): PerformResult =
      state.result = value
      null

  final case class Ap[T, U](ff: Manip[T => U], fa: Manip[T])
      extends Manip[U],
        StackRec:
    def performImpl(state: PerformState): PerformResult =
      state.leftInc()
      state.speculateStack.push(this)
      ff
    def performPop(state: PerformState): PerformResult =
      state.leftDec()
      state.speculateStack.push(
        PerformState.MapFn(state.result.asInstanceOf[T => U])
      )
      state.result = null
      fa
    def performBacktrack(
        state: PerformState,
        posInfo: DebugInfo
    ): PerformResult =
      state.leftDec()
      null
    def performCommit(state: PerformState): Unit =
      state.leftInc()
      state.committedStack.push(this)

  final case class MapOpt[T, U](manip: Manip[T], fn: T => U)
      extends Manip[U],
        NopBacktrack,
        PushCommit:
    def performImpl(state: PerformState): PerformResult =
      state.speculateStack.push(this)
      manip
    def performPop(state: PerformState): PerformResult =
      state.result = fn(state.result.asInstanceOf)
      null

  final case class FlatMap[T, U](manip: Manip[T], fn: T => Manip[U])
      extends Manip[U],
        StackRec:
    def performImpl(state: PerformState): PerformResult =
      state.leftInc()
      state.speculateStack.push(this)
      manip
    def performPop(state: PerformState): PerformResult =
      state.leftDec()
      val next = fn(state.result.asInstanceOf[T])
      state.result = null
      next
    def performBacktrack(
        state: PerformState,
        posInfo: DebugInfo
    ): PerformResult =
      state.leftDec()
      null
    def performCommit(state: PerformState): Unit =
      state.leftInc()
      state.committedStack.push(this)

  final case class Restrict[T, U](
      manip: Manip[T],
      restriction: PartialFunction[T, U],
      debugInfo: DebugInfo
  ) extends Manip[U],
        NopBacktrack,
        PushCommit:
    def performImpl(state: PerformState): PerformResult =
      state.speculateStack.push(this)
      manip
    def performPop(state: PerformState): PerformResult =
      val value = state.result.asInstanceOf[T]
      value match
        case restriction(result) =>
          state.result = result
          null
        case _ =>
          state.backtrack(this, debugInfo)

  final case class Effect[T](fn: () => T) extends Manip[T]:
    def performImpl(state: PerformState): PerformResult =
      state.result = fn()
      null

  final case class Finally[T](manip: Manip[T], fn: () => Unit)
      extends Manip[T],
        StackRec,
        PushCommit:
    def performImpl(state: PerformState): PerformResult =
      state.speculateStack.push(this)
      manip
    def performPop(state: PerformState): PerformResult =
      fn()
      null
    def performBacktrack(
        state: PerformState,
        posInfo: DebugInfo
    ): PerformResult =
      fn()
      null

  final case class KeepLeft[T](left: Manip[T], right: Manip[?])
      extends Manip[T],
        StackRec:
    def performImpl(state: PerformState): PerformResult =
      state.leftInc()
      state.speculateStack.push(this)
      left
    def performPop(state: PerformState): PerformResult =
      state.leftDec()
      val storedResult = state.result
      state.speculateStack.push(PerformState.MapFn(_ => storedResult))
      state.result = null
      right
    def performBacktrack(
        state: PerformState,
        posInfo: DebugInfo
    ): PerformResult =
      state.leftDec()
      null
    def performCommit(state: PerformState): Unit =
      state.leftInc()
      state.committedStack.push(this)

  final case class KeepRight[T](left: Manip[?], right: Manip[T])
      extends Manip[T],
        StackRec:
    def performImpl(state: PerformState): PerformResult =
      state.leftInc()
      state.speculateStack.push(this)
      left
    def performPop(state: PerformState): PerformResult =
      state.leftDec()
      state.result = null
      right
    def performBacktrack(
        state: PerformState,
        posInfo: DebugInfo
    ): PerformResult =
      state.leftDec()
      null
    def performCommit(state: PerformState): Unit =
      state.leftInc()
      state.committedStack.push(this)

  final case class Commit[T](manip: Manip[T], debugInfo: DebugInfo)
      extends Manip[T]:
    def performImpl(state: PerformState): PerformResult =
      state.tracer.onCommit(this, debugInfo)
      state.startCommit(debugInfo)
      state.speculateStack.consumeInReverse(_.performCommit(state))
      state.finishCommit()
      manip

  final case class RefInit[T, U](
      ref: Manip.Ref[T],
      initFn: () => T,
      manip: Manip[U],
      debugInfo: DebugInfo
  ) extends Manip[U]:
    def performImpl(state: PerformState): PerformResult =
      state.refMap.get(ref) match
        case Some(_) =>
          state.backtrack(this, debugInfo)
        case None =>
          val initValue = initFn()
          state.tracer.onAssign(this, ref, initValue)
          state.refMap(ref) = initValue
          state.pushSaveDel(ref)
          manip

  final case class RefReset[T, U](
      ref: Manip.Ref[T],
      manip: Manip[U],
      debugInfo: DebugInfo
  ) extends Manip[U]:
    def performImpl(state: PerformState): PerformResult =
      state.refMap.get(ref) match
        case None =>
        case Some(value) =>
          state.pushSave(ref, value)
          state.tracer.onDel(this, ref, debugInfo)
          state.refMap.remove(ref)

      manip

  final case class RefGet[T](ref: Manip.Ref[T], debugInfo: DebugInfo)
      extends Manip[T]:
    def performImpl(state: PerformState): PerformResult =
      state.refMap.get(ref) match
        case None =>
          state.backtrack(this, debugInfo)
        case Some(value) =>
          state.tracer.onRead(this, ref, value, debugInfo)
          state.result = value
          null

  final case class RefUpdated[T, U](
      ref: Manip.Ref[T],
      fn: T => T,
      manip: Manip[U],
      debugInfo: DebugInfo
  ) extends Manip[U]:
    def performImpl(state: PerformState): PerformResult =
      state.refMap.get(ref) match
        case None => state.backtrack(this, debugInfo)
        case Some(value) =>
          state.tracer.onRead(this, ref, value, debugInfo)
          state.pushSave(ref, value)
          val newValue = fn(value)
          state.tracer.onAssign(this, ref, newValue)
          state.refMap(ref) = newValue
          manip

  case object GetTracer extends Manip[Tracer]:
    def performImpl(state: PerformState): PerformResult =
      state.result = state.tracer
      null

  final case class Disjunction[T](
      first: Manip[T],
      second: Manip[T],
      debugInfo: DebugInfo
  ) extends Manip[T],
        StackRec:
    def performImpl(state: PerformState): PerformResult =
      state.leftInc()
      state.tracer.onBranch(this, debugInfo)
      state.speculateStack.push(this)
      first
    def performPop(state: PerformState): PerformResult =
      state.leftDec()
      null
    def performBacktrack(
        state: PerformState,
        posInfo: DebugInfo
    ): PerformResult =
      state.leftDec()
      second
    def performCommit(state: PerformState): Unit =
      () // drop this, do not leftInc

  final case class Deferred[T](fn: () => Manip[T]) extends Manip[T]:
    def performImpl(state: PerformState): PerformResult = fn()

  final case class TapEffect[T](manip: Manip[T], fn: T => Unit)
      extends Manip[T],
        NopBacktrack,
        PushCommit:
    def performImpl(state: PerformState): PerformResult =
      state.speculateStack.push(this)
      manip
    def performPop(state: PerformState): PerformResult =
      fn(state.result.asInstanceOf[T])
      null

  final case class RestrictHandle[T](
      fn: PartialFunction[Handle, Handle],
      manip: Manip[T],
      debugInfo: DebugInfo
  ) extends Manip[T]:
    def performImpl(state: PerformState): PerformResult =
      state.refMap.get(Handle.ref) match
        case None => state.backtrack(this, debugInfo)
        case Some(oldHandle) =>
          state.tracer.onRead(this, Handle.ref, oldHandle, debugInfo)
          oldHandle match
            case fn(handle) =>
              state.pushSave(Handle.ref, oldHandle)
              state.tracer.onAssign(this, Handle.ref, handle)
              state.refMap(Handle.ref) = handle
              manip
            case _ =>
              state.backtrack(this, debugInfo)

  abstract class Ref[T] extends RefMapFactory.Mapped:
    final def init[U](using DebugInfo)(init: => T)(manip: Manip[U]): Manip[U] =
      Manip.RefInit(this, () => init, manip, summon[DebugInfo])
    final def reset[U](using DebugInfo)(manip: Manip[U]): Manip[U] =
      Manip.RefReset(this, manip, summon[DebugInfo])
    final def get(using DebugInfo): Manip[T] =
      Manip.RefGet(this, summon[DebugInfo])
    final def updated[U](using
        DebugInfo
    )(fn: T => T)(
        manip: Manip[U]
    ): Manip[U] = Manip.RefUpdated(this, fn, manip, summon[DebugInfo])
    final def doEffect[U](using DebugInfo)(fn: T => U): Manip[U] =
      get.lookahead.flatMap: v =>
        effect(fn(v))

  object RefMapFactory extends SymbolicMapFactory

  final class RefMap(private val map: RefMapFactory.Map[Any]) extends AnyVal:
    def apply[T](ref: Ref[T]): T =
      map(ref).nn.asInstanceOf[T]

    def get[T](ref: Ref[T]): Option[T] =
      map(ref) match
        case null  => None
        case value => Some(value.asInstanceOf[T])

    def updated[T](ref: Ref[T], value: T): RefMap =
      RefMap(map.updated(ref, value))

    def removed[T](ref: Ref[T]): RefMap =
      RefMap(map.removed(ref))

    def update[T](ref: Ref[T], value: T): Unit =
      map(ref) = value

    def remove[T](ref: Ref[T]): Unit =
      map.remove(ref)

  object RefMap:
    def empty: RefMap = RefMap(RefMapFactory.Map.empty(null))

  type Rules = Manip[RulesResult]

  enum RulesResult:
    case Progress, NoProgress

  val unit: Manip[Unit] = ().pure
  // export dsl.defer
  export applicative.pure

  given applicative: cats.Applicative[Manip] with
    override def unit: Manip[Unit] = Manip.unit
    override def map[A, B](fa: Manip[A])(f: A => B): Manip[B] =
      Manip.MapOpt(fa, f)
    def ap[A, B](ff: Manip[A => B])(fa: Manip[A]): Manip[B] =
      Manip.Ap(ff, fa)
    def pure[A](x: A): Manip[A] = Manip.Pure(x)
    override def as[A, B](fa: Manip[A], b: B): Manip[B] =
      productR(fa)(pure(b))
    override def productL[A, B](fa: Manip[A])(fb: Manip[B]): Manip[A] =
      Manip.KeepLeft(fa, fb)
    override def productR[A, B](fa: Manip[A])(fb: Manip[B]): Manip[B] =
      Manip.KeepRight(fa, fb)

  given monoidK(using DebugInfo): cats.MonoidK[Manip] with
    def empty[A]: Manip[A] = Manip.Backtrack(summon[DebugInfo])
    def combineK[A](x: Manip[A], y: Manip[A]): Manip[A] =
      (x, y) match
        case (Manip.Backtrack(_), right) => right
        case (left, Manip.Backtrack(_))  => left
        // case (
        //       Manip.Negated(manip1, debugInfo1),
        //       Manip.Negated(manip2, debugInfo2)
        //     ) =>
        //   Manip.Negated(combineK(manip1, manip2), debugInfo1 ++ debugInfo2)
        case _ =>
          Manip.Disjunction(x, y, summon[DebugInfo])

  class Lookahead[T](val manip: Manip[T]) extends AnyVal:
    def flatMap[U](fn: T => Manip[U]): Manip[U] =
      Manip.FlatMap(manip, fn)

  object Lookahead:
    given applicative: cats.Applicative[Lookahead] with
      def ap[A, B](ff: Lookahead[A => B])(fa: Lookahead[A]): Lookahead[B] =
        Lookahead(ff.manip.ap(fa.manip))
      def pure[A](x: A): Lookahead[A] =
        Lookahead(Manip.pure(x))
    given monoidK(using DebugInfo): cats.MonoidK[Lookahead] with
      def empty[A]: Lookahead[A] = Lookahead(Manip.monoidK.empty)
      def combineK[A](x: Lookahead[A], y: Lookahead[A]): Lookahead[A] =
        Lookahead(x.manip.combineK(y.manip))
end Manip
