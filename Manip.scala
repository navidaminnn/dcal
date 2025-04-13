// Copyright 2024-2025 DCal Team
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

package distcompiler

import cats.syntax.all.given
import util.{++, toShortString}
import com.github.difflib.{DiffUtils, UnifiedDiffUtils}
import scala.jdk.CollectionConverters._
import scala.annotation.tailrec
import scala.util.NotGiven
import scala.reflect.ClassTag

sealed abstract class Manip[+T]:
  private inline given DebugInfo = DebugInfo.poison

  def isBacktrack: Boolean = false

  private[Manip] def performImpl(state: Manip.PerformState): Manip.PerformResult

  def perform(using DebugInfo)(): T =
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
    then
      Manip.ops.instrumentWithTracerReentrant(ManipReferenceTracer(this))(impl)
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
  private var tracerVar: ThreadLocal[Tracer] = ThreadLocal()

  // TODO: add tailrec detection. could do it by counting how many non-tailrec nodes are pending, if 0 then tailrec
  //       ... question: if we saved everything re: a given non tailrec already, we can smush the intermediate saves
  //       so really, there's a tailrec epoch per refMap cell, and if our tailrec level == that epoch, no need to save value

  final class SegmentedStack[T <: AnyRef](using NotGiven[Array[?] <:< T]):
    inline val stackSegmentSize = 16
    private var stack = Array.ofDim[AnyRef](stackSegmentSize)
    private var base = stack
    private var stackSize = 0

    def consumeInReverse(fn: T => Unit): Unit =
      var idx = 0
      while base ne stack do
        base(idx).nn match
          case next: Array[AnyRef] if idx == 0 =>
            idx += 1 // back ref, skip
          case next: Array[AnyRef] if idx == base.length - 1 =>
            base = next
            idx = 0
          case elem =>
            fn(elem.asInstanceOf[T])
            idx += 1
      end while

      while idx < stackSize do
        base(idx).nn match
          case _: Array[AnyRef] if idx == 0 => // skip
          case elem =>
            fn(elem.asInstanceOf[T])
        base(idx) = null.asInstanceOf[AnyRef]
        idx += 1
      end while
      stackSize = 0
    end consumeInReverse

    def push(rec: T): Unit =
      if stackSize == stack.length
      then
        val oldStack = stack
        val prevElem = stack(stackSize - 1)
        stack = Array.ofDim[AnyRef](stackSegmentSize)
        oldStack(stackSize - 1) = stack
        stack(0) = oldStack
        stack(1) = prevElem
        stackSize = 2

      stack(stackSize) = rec
      stackSize += 1

    @tailrec
    def pop(): T | Null =
      if stackSize == 0
      then null
      else
        stackSize -= 1
        stack(stackSize) match
          case lowerStack: Array[AnyRef] =>
            stack = lowerStack
            stack(stack.length - 1) = null.asInstanceOf[AnyRef] // drop next-ptr
            stackSize = stack.length - 1
            pop()
          case rec => rec.asInstanceOf[T]
  end SegmentedStack

  private final class PerformState(var rootDebugInfo: DebugInfo):
    var result: Any | Null = null
    var refMap = RefMap.empty
    val committedStack = SegmentedStack[PerformState.StackRec]()
    val speculateStack = SegmentedStack[PerformState.StackRec]()

    val tracer: Tracer = tracerVar.get() match
      case null   => NopTracer
      case tracer => tracer

    def backtrack(self: Manip[?], posInfo: DebugInfo): PerformResult =
      tracer.onBacktrack(self, posInfo)(using refMap)
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
        if popped eq null
        then popped = committedStack.pop()

        popped match
          case null => return null
          case elem =>
            popResult = elem.performPop(this)
      popResult
  end PerformState

  object PerformState:
    sealed trait StackRec:
      private[Manip] def performPop(
          state: Manip.PerformState
      ): Manip.PerformResult
      private[Manip] def performBacktrack(
          state: Manip.PerformState,
          posInfo: DebugInfo
      ): Manip.PerformResult

    sealed trait StackRecNopBacktrack extends StackRec:
      private[Manip] def performBacktrack(
          state: PerformState,
          posInfo: DebugInfo
      ): PerformResult = null

    sealed trait StackRecNopPop extends StackRec:
      private[Manip] def performPop(
          state: Manip.PerformState
      ): Manip.PerformResult = null

    final case class ResetRef[T](ref: Ref[T], value: T) extends StackRec:
      private[Manip] def performBacktrack(
          state: PerformState,
          posInfo: DebugInfo
      ): PerformResult =
        state.refMap = state.refMap.updated(ref, value)
        null
      private[Manip] def performPop(state: PerformState): PerformResult =
        state.refMap = state.refMap.updated(ref, value)
        null

    final class MapFn[T, U](fn: T => U) extends StackRecNopBacktrack:
      private[Manip] def performPop(state: PerformState): PerformResult =
        state.result = fn(state.result.asInstanceOf[T])
        null

  import PerformState.*

  private type PerformResult = Manip[?] | Null

  final case class Backtrack(debugInfo: DebugInfo) extends Manip[Nothing]:
    override def isBacktrack: Boolean = true
    private[Manip] def performImpl(state: PerformState): PerformResult =
      state.backtrack(this, debugInfo)

  final case class Pure[T](value: T) extends Manip[T]:
    private[Manip] def performImpl(state: PerformState): PerformResult =
      state.result = value
      null

  final case class Ap[T, U](ff: Manip[T => U], fa: Manip[T])
      extends Manip[U],
        StackRecNopBacktrack:
    private[Manip] def performImpl(state: PerformState): PerformResult =
      state.speculateStack.push(this)
      ff
    private[Manip] def performPop(state: PerformState): PerformResult =
      state.speculateStack.push(
        PerformState.MapFn(state.result.asInstanceOf[T => U])
      )
      state.result = null
      fa

  final case class MapOpt[T, U](manip: Manip[T], fn: T => U)
      extends Manip[U],
        StackRecNopBacktrack:
    private[Manip] def performImpl(state: PerformState): PerformResult =
      state.speculateStack.push(this)
      manip
    private[Manip] def performPop(state: PerformState): PerformResult =
      state.result = fn(state.result.asInstanceOf)
      null

  final case class FlatMap[T, U](manip: Manip[T], fn: T => Manip[U])
      extends Manip[U],
        StackRecNopBacktrack:
    private[Manip] def performImpl(state: PerformState): PerformResult =
      state.speculateStack.push(this)
      manip
    private[Manip] def performPop(state: PerformState): PerformResult =
      val next = fn(state.result.asInstanceOf[T])
      state.result = null
      next

  final case class Restrict[T, U](
      manip: Manip[T],
      restriction: PartialFunction[T, U],
      debugInfo: DebugInfo
  ) extends Manip[U],
        StackRecNopBacktrack:
    private[Manip] def performImpl(state: PerformState): PerformResult =
      state.speculateStack.push(this)
      manip
    private[Manip] def performPop(state: PerformState): PerformResult =
      val value = state.result.asInstanceOf[T]
      value match
        case restriction(result) =>
          state.result = result
          null
        case _ =>
          state.backtrack(this, debugInfo)

  final case class Effect[T](fn: () => T) extends Manip[T]:
    private[Manip] def performImpl(state: PerformState): PerformResult =
      state.result = fn()
      null

  final case class Finally[T](manip: Manip[T], fn: () => Unit)
      extends Manip[T],
        StackRec:
    private[Manip] def performImpl(state: PerformState): PerformResult =
      state.speculateStack.push(this)
      manip
    private[Manip] def performPop(state: PerformState): PerformResult =
      fn()
      null
    private[Manip] def performBacktrack(
        state: PerformState,
        posInfo: DebugInfo
    ): PerformResult =
      fn()
      null

  final case class KeepLeft[T](left: Manip[T], right: Manip[?])
      extends Manip[T],
        StackRecNopBacktrack:
    private[Manip] def performImpl(state: PerformState): PerformResult =
      state.speculateStack.push(this)
      left
    private[Manip] def performPop(state: PerformState): PerformResult =
      val storedResult = state.result
      state.speculateStack.push(PerformState.MapFn(_ => storedResult))
      state.result = null
      right

  final case class KeepRight[T](left: Manip[?], right: Manip[T])
      extends Manip[T],
        StackRecNopBacktrack:
    private[Manip] def performImpl(state: PerformState): PerformResult =
      state.speculateStack.push(this)
      left
    private[Manip] def performPop(state: PerformState): PerformResult =
      state.result = null
      right

  final case class Commit[T](manip: Manip[T], debugInfo: DebugInfo)
      extends Manip[T]:
    private[Manip] def performImpl(state: PerformState): PerformResult =
      state.tracer.onCommit(this, debugInfo)(using state.refMap)
      state.rootDebugInfo = debugInfo
      state.speculateStack.consumeInReverse:
        case elem: StackRecNopPop => // drop (includes Disjunction)
        case elem: StackRec       => state.committedStack.push(elem)
      manip

  final case class RefInit[T, U](
      ref: Manip.Ref[T],
      initFn: () => T,
      manip: Manip[U],
      debugInfo: DebugInfo
  ) extends Manip[U],
        StackRec:
    private[Manip] def performImpl(state: PerformState): PerformResult =
      state.refMap.get(ref) match
        case Some(_) =>
          state.backtrack(this, debugInfo)
        case None =>
          val initValue = initFn()
          state.tracer.onAssign(this, ref, initValue)(using state.refMap)
          state.refMap = state.refMap.updated(ref, initValue)
          state.speculateStack.push(this)
          manip
    override private[Manip] def performPop(state: PerformState): PerformResult =
      state.tracer.onDel(this, ref, debugInfo)(using state.refMap)
      state.refMap = state.refMap.removed(ref)
      null
    override private[Manip] def performBacktrack(
        state: PerformState,
        posInfo: DebugInfo
    ): PerformResult =
      state.tracer.onDel(this, ref, debugInfo)(using state.refMap)
      state.refMap = state.refMap.removed(ref)
      null

  final case class RefReset[T, U](
      ref: Manip.Ref[T],
      manip: Manip[U],
      debugInfo: DebugInfo
  ) extends Manip[U]:
    private[Manip] def performImpl(state: PerformState): PerformResult =
      state.refMap.get(ref) match
        case None =>
        case Some(_) =>
          state.tracer.onDel(this, ref, debugInfo)(using state.refMap)
          state.refMap = state.refMap.removed(ref)

      manip

  final case class RefGet[T](ref: Manip.Ref[T], debugInfo: DebugInfo)
      extends Manip[T]:
    private[Manip] def performImpl(state: PerformState): PerformResult =
      state.refMap.get(ref) match
        case None =>
          state.backtrack(this, debugInfo)
        case Some(value) =>
          state.tracer.onRead(this, ref, value, debugInfo)(using state.refMap)
          state.result = value
          null

  final case class RefUpdated[T, U](
      ref: Manip.Ref[T],
      fn: T => T,
      manip: Manip[U],
      debugInfo: DebugInfo
  ) extends Manip[U]:
    private[Manip] def performImpl(state: PerformState): PerformResult =
      state.refMap.get(ref) match
        case None => state.backtrack(this, debugInfo)
        case Some(value) =>
          state.tracer.onRead(this, ref, value, debugInfo)(using state.refMap)
          state.speculateStack.push(PerformState.ResetRef(ref, value))
          val newValue = fn(value)
          state.tracer.onAssign(this, ref, newValue)(using state.refMap)
          state.refMap = state.refMap.updated(ref, newValue)
          manip

  case object GetRefMap extends Manip[Manip.RefMap]:
    private[Manip] def performImpl(state: PerformState): PerformResult =
      state.result = state.refMap
      null

  case object GetTracer extends Manip[Manip.Tracer]:
    private[Manip] def performImpl(state: PerformState): PerformResult =
      state.result = state.tracer
      null

  final case class Disjunction[T](
      first: Manip[T],
      second: Manip[T],
      debugInfo: DebugInfo
  ) extends Manip[T],
        StackRecNopPop:
    private[Manip] def performImpl(state: PerformState): PerformResult =
      state.tracer.onBranch(this, debugInfo)(using state.refMap)
      state.speculateStack.push(this)
      first
    private[Manip] def performBacktrack(
        state: PerformState,
        posInfo: DebugInfo
    ): PerformResult =
      second

  final case class Deferred[T](fn: () => Manip[T]) extends Manip[T]:
    private[Manip] def performImpl(state: PerformState): PerformResult = fn()

  final case class TapEffect[T](manip: Manip[T], fn: T => Unit)
      extends Manip[T],
        StackRecNopBacktrack:
    private[Manip] def performImpl(state: PerformState): PerformResult =
      state.speculateStack.push(this)
      manip
    private[Manip] def performPop(state: PerformState): PerformResult =
      fn(state.result.asInstanceOf[T])
      null

  final case class RestrictHandle[T](
      fn: PartialFunction[Manip.Handle, Manip.Handle],
      manip: Manip[T],
      debugInfo: DebugInfo
  ) extends Manip[T]:
    private[Manip] def performImpl(state: PerformState): PerformResult =
      state.refMap.get(Manip.Handle.ref) match
        case None => state.backtrack(this, debugInfo)
        case Some(oldHandle) =>
          state.tracer.onRead(this, Manip.Handle.ref, oldHandle, debugInfo)(
            using state.refMap
          )
          oldHandle match
            case fn(handle) =>
              state.speculateStack.push(
                PerformState.ResetRef(Manip.Handle.ref, oldHandle)
              )
              state.tracer.onAssign(this, Manip.Handle.ref, handle)(using
                state.refMap
              )
              state.refMap = state.refMap.updated(Manip.Handle.ref, handle)
              manip
            case _ =>
              state.backtrack(this, debugInfo)

  abstract class Ref[T]:
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
      import ops.{lookahead, effect}
      get.lookahead.flatMap: v =>
        effect(fn(v))

    override def equals(that: Any): Boolean = this eq that.asInstanceOf[AnyRef]
    override def hashCode(): Int = System.identityHashCode(this)

  final class RefMap(private val map: Map[Manip.Ref[?], Any]) extends AnyVal:
    def get[T](ref: Ref[T]): Option[T] =
      map.get(ref).asInstanceOf[Option[T]]

    def updated[T](ref: Ref[T], value: T): RefMap =
      RefMap(map.updated(ref, value))

    def removed[T](ref: Ref[T]): RefMap =
      RefMap(map.removed(ref))

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

    def treeDescrShort: String =
      treeDescr.toShortString()

  object RefMap:
    def empty: RefMap = RefMap(Map.empty)

  type Rules = Manip[RulesResult]

  enum RulesResult:
    case Progress, NoProgress

  val unit: Manip[Unit] = ().pure
  export ops.defer
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

  trait Tracer extends java.io.Closeable:
    def beforePass(debugInfo: DebugInfo)(using RefMap): Unit
    def afterPass(debugInfo: DebugInfo)(using RefMap): Unit
    def onRead(manip: Manip[?], ref: Ref[?], value: Any, debugInfo: DebugInfo)(
        using RefMap
    ): Unit
    def onAssign(manip: Manip[?], ref: Ref[?], value: Any)(using RefMap): Unit
    def onDel(manip: Manip[?], ref: Ref[?], debugInfo: DebugInfo)(using
        RefMap
    ): Unit
    def onBranch(manip: Manip[?], debugInfo: DebugInfo)(using RefMap): Unit
    def onCommit(manip: Manip[?], debugInfo: DebugInfo)(using RefMap): Unit
    def onBacktrack(manip: Manip[?], debugInfo: DebugInfo)(using RefMap): Unit
    def onFatal(manip: Manip[?], debugInfo: DebugInfo, from: DebugInfo)(using
        RefMap
    ): Unit
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

  abstract class AbstractNopTracer extends Tracer:
    def beforePass(debugInfo: DebugInfo)(using RefMap): Unit = ()
    def afterPass(debugInfo: DebugInfo)(using RefMap): Unit = ()
    def onRead(manip: Manip[?], ref: Ref[?], value: Any, debugInfo: DebugInfo)(
        using RefMap
    ): Unit = ()
    def onAssign(manip: Manip[?], ref: Ref[?], value: Any)(using RefMap): Unit =
      ()
    def onDel(manip: Manip[?], ref: Ref[?], debugInfo: DebugInfo)(using
        RefMap
    ): Unit = ()
    def onBranch(manip: Manip[?], debugInfo: DebugInfo)(using RefMap): Unit = ()
    def onCommit(manip: Manip[?], debugInfo: DebugInfo)(using RefMap): Unit = ()
    def onBacktrack(manip: Manip[?], debugInfo: DebugInfo)(using RefMap): Unit =
      ()
    def onFatal(manip: Manip[?], debugInfo: DebugInfo, from: DebugInfo)(using
        RefMap
    ): Unit = ()
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

  object NopTracer extends AbstractNopTracer

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
      summon[RefMap].treeDescrShort

    def beforePass(debugInfo: DebugInfo)(using RefMap): Unit =
      logln(s"pass init $debugInfo at $treeDesc")

    def afterPass(debugInfo: DebugInfo)(using RefMap): Unit =
      logln(s"pass end $debugInfo at $treeDesc")

    def onRead(manip: Manip[?], ref: Ref[?], value: Any, debugInfo: DebugInfo)(
        using RefMap
    ): Unit =
      value match
        case value: AnyVal =>
          logln(s"read $ref --> $value at $treeDesc")
        case value: AnyRef =>
          logln(s"read $ref --> ${value.toShortString()} at $treeDesc")

    def onAssign(manip: Manip[?], ref: Ref[?], value: Any)(using RefMap): Unit =
      value match
        case value: AnyVal =>
          logln(s"assign $ref <-- ${value} at $treeDesc")
        case value: AnyRef =>
          logln(s"assign $ref <-- ${value.toShortString} at $treeDesc")

    def onDel(manip: Manip[?], ref: Ref[?], debugInfo: DebugInfo)(using
        RefMap
    ): Unit =
      logln(s"del $ref $debugInfo")

    def onBranch(manip: Manip[?], debugInfo: DebugInfo)(using RefMap): Unit =
      logln(s"branch $debugInfo at $treeDesc")

    def onCommit(manip: Manip[?], debugInfo: DebugInfo)(using RefMap): Unit =
      logln(s"commit $debugInfo at $treeDesc")

    def onBacktrack(manip: Manip[?], debugInfo: DebugInfo)(using RefMap): Unit =
      logln(s"backtrack $debugInfo at $treeDesc")

    def onFatal(manip: Manip[?], debugInfo: DebugInfo, from: DebugInfo)(using
        RefMap
    ): Unit =
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

  final class RewriteDebugTracer(debugFolder: os.Path)
      extends AbstractNopTracer:
    os.remove.all(debugFolder) // no confusing left-over files!
    os.makeDir.all(debugFolder)
    private var passCounter = 0
    private var rewriteCounter = 0

    private def treeDesc(using RefMap): geny.Writable =
      summon[RefMap].get(Manip.Handle.ref) match
        case None => "<no tree>"
        case Some(handle) =>
          handle.findTop match
            case None => "<??? top not found ???>"
            case Some(top) =>
              top.toPrettyWritable(Wellformed.empty)

    private def pathAt(
        passIdx: Int,
        rewriteIdx: Int,
        isDiff: Boolean = false
    ): os.Path =
      val ext = if isDiff then ".diff" else ".txt"
      debugFolder / f"$passIdx%03d" / f"$rewriteIdx%03d$ext%s"

    private def currPath: os.Path =
      pathAt(passCounter, rewriteCounter)

    private def prevPath: os.Path =
      pathAt(passCounter, rewriteCounter - 1)

    private def diffPath: os.Path =
      pathAt(passCounter, rewriteCounter, isDiff = true)

    private def takeSnapshot(debugInfo: DebugInfo)(using RefMap): Unit =
      os.write(
        target = currPath,
        data = (s"//! $debugInfo\n": geny.Writable) ++ treeDesc ++ "\n",
        createFolders = true
      )

    private def takeDiff()(using RefMap): Unit =
      val prevLines = os.read.lines(prevPath).asJava
      val currLines = os.read.lines(currPath).asJava
      val patch = DiffUtils.diff(prevLines, currLines)
      val unifiedDiff = UnifiedDiffUtils.generateUnifiedDiff(
        prevPath.toString(),
        currPath.toString(),
        prevLines,
        patch,
        3
      )

      os.write.over(
        diffPath,
        unifiedDiff.asScala.mkString("\n")
      )

    override def beforePass(debugInfo: DebugInfo)(using RefMap): Unit =
      passCounter += 1
      rewriteCounter = 0
      takeSnapshot(debugInfo)

    override def afterPass(debugInfo: DebugInfo)(using RefMap): Unit =
      rewriteCounter += 1
      takeSnapshot(debugInfo)

    override def onRewriteComplete(
        debugInfo: DebugInfo,
        parent: Node.Parent,
        idx: Int,
        resultCount: Int
    )(using RefMap): Unit =
      rewriteCounter += 1
      takeSnapshot(debugInfo)
      takeDiff()

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
            throw NodeError(
              s"mismatch: idx $idx in ${parent.toShortString()} and ptr -> ${child.toShortString()}"
            )
        case Sentinel(parent, idx) =>
          if parent.children.length != idx
          then
            throw NodeError(
              s"mismatch: sentinel at $idx does not point to end ${parent.children.length} of ${parent.toShortString()}"
            )

    def rightSibling: Option[Handle] =
      assertCoherence()
      this match
        case AtTop(_) => None
        case AtChild(parent, idx, _) =>
          Handle.idxIntoParent(parent, idx + 1)
        case Sentinel(_, _) => None

    def leftSibling: Option[Handle] =
      assertCoherence()
      this match
        case AtTop(_) => None
        case AtChild(parent, idx, _) =>
          Handle.idxIntoParent(parent, idx - 1)
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

    def findLastChild: Option[Handle] =
      assertCoherence()
      def forParent(parent: Node.Parent): Option[Handle] =
        parent.children.indices.lastOption
          .flatMap(Handle.idxIntoParent(parent, _))

      this match
        case AtTop(top) => forParent(top)
        case AtChild(_, _, child) =>
          child match
            case child: Node.Parent => forParent(child)
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

    def atIdxFromRight(idx: Int): Option[Handle] =
      assertCoherence()
      this match
        case AtTop(_) => None
        case handle: (Sentinel | AtChild) =>
          val parent = handle.parent
          Handle.idxIntoParent(parent, parent.children.size - 1 - idx)

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

    def findTop: Option[Node.Top] =
      def forParent(parent: Node.Parent): Option[Node.Top] =
        val p = parent
        inline given DebugInfo = DebugInfo.notPoison
        import dsl.*
        p.inspect:
          on(theTop).value
            | atAncestor(on(theTop).value)
      this match
        case AtTop(top)            => Some(top)
        case AtChild(parent, _, _) => forParent(parent)
        case Sentinel(parent, _)   => forParent(parent)

  object Handle:
    object ref extends Ref[Handle]

    def fromNode(node: Node.All): Handle =
      node match
        case top: Node.Top => Handle.AtTop(top)
        case child: Node.Child =>
          require(child.parent.nonEmpty, "node must have parent")
          Handle.AtChild(child.parent.get, child.idxInParent, child)

    private def idxIntoParent(parent: Node.Parent, idx: Int): Option[Handle] =
      if parent.children.isDefinedAt(idx)
      then Some(AtChild(parent, idx, parent.children(idx)))
      else if idx == parent.children.length
      then Some(Sentinel(parent, idx))
      else None

    extension (handle: Handle.Sentinel | Handle.AtChild)
      def idx: Int = handle match
        case Sentinel(_, idx)   => idx
        case AtChild(_, idx, _) => idx

      def parent: Node.Parent = handle match
        case Sentinel(parent, _)   => parent
        case AtChild(parent, _, _) => parent

  object ops:
    def instrumentWithTracer[T](tracer: Tracer)(fn: => T): T =
      require(
        tracerVar.get() eq null,
        s"tried to set over existing tracer ${tracerVar.get()}"
      )
      tracerVar.set(tracer)
      try fn
      finally
        tracer.close()
        assert(
          tracerVar.get() eq tracer,
          s"tracer $tracer was replaced by ${tracerVar.get()} during execution"
        )
        tracerVar.remove()

    def instrumentWithTracerReentrant[T, Tr <: Tracer: ClassTag](tracer: Tr)(
        fn: => T
    ): T =
      tracerVar.get() match
        case null => instrumentWithTracer(tracer)(fn)
        case existingTracer: Tr =>
          tracerVar.remove()
          try instrumentWithTracer(tracer)(fn)
          finally tracerVar.set(existingTracer)
        case existingTracer: Tracer =>
          throw IllegalArgumentException(
            s"re-entrant tracer $tracer is not of the same type as existing tracer $existingTracer"
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
        DebugInfo
    )(node: Node.All)(manip: Manip[T]): Manip[T] =
      initHandle(Handle.fromNode(node))(manip)

    def initHandle[T](using
        DebugInfo
    )(handle: Manip.Handle)(manip: Manip[T]): Manip[T] =
      Manip.Handle.ref.init(handle)(manip)

    def atNode[T](using DebugInfo)(node: Node.All)(manip: Manip[T]): Manip[T] =
      atHandle(Handle.fromNode(node))(manip)

    def atHandle[T](using
        DebugInfo
    )(handle: Manip.Handle)(manip: Manip[T]): Manip[T] =
      restrictHandle(manip)(_ => Some(handle))

    def getHandle(using DebugInfo): Manip[Manip.Handle] =
      Manip.Handle.ref.get

    def getTracer: Manip[Manip.Tracer] =
      Manip.GetTracer

    def restrictHandle[T](using
        DebugInfo
    )(manip: Manip[T])(fn: PartialFunction[Handle, Handle]): Manip[T] =
      Manip.RestrictHandle(fn, manip, summon[DebugInfo])

    def restrictHandle[T](using
        DebugInfo
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
        DebugInfo
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
        DebugInfo.Ctx
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
          DebugInfo.Ctx
      )(
          action: on.Ctx ?=> T => Rules
      ): Rules =
        (raw, getHandle, getTracer, Manip.GetRefMap).tupled
          .flatMap: (patResult, handle, tracer, refMap) =>
            handle.assertCoherence()
            val value = patResult.value
            val (parent, startIdx) =
              handle match
                case Handle.AtTop(top) =>
                  throw NodeError(
                    s"tried to rewrite top ${top.toShortString()}"
                  )
                case Handle.AtChild(parent, idx, _) => (parent, idx)
                case Handle.Sentinel(parent, idx)   => (parent, idx)
            val matchedCount =
              patResult match
                case SeqPattern.Result.Top(top, _) =>
                  throw NodeError(
                    s"ended pattern at top ${top.toShortString()}"
                  )
                case patResult: (SeqPattern.Result.Look[T] |
                      SeqPattern.Result.Match[T]) =>
                  assert(patResult.parent eq parent)
                  if patResult.isMatch
                  then patResult.idx - startIdx + 1
                  else patResult.idx - startIdx

            assert(
              matchedCount >= 1,
              "can't rewrite based on a pattern that matched nothing"
            )
            tracer.onRewriteMatch(
              summon[DebugInfo],
              parent,
              startIdx,
              matchedCount
            )(using refMap)
            action(using on.Ctx(matchedCount))(value)

    object on:
      final case class Ctx(matchedCount: Int) extends AnyVal

    def spliceThen(using
        DebugInfo.Ctx
    )(using
        onCtx: on.Ctx
    )(
        nodes: Iterable[Node.Child]
    )(
        manip: on.Ctx ?=> Rules
    ): Rules =
      // Preparation for the rewrite may have reparented the node we were "on"
      // When you immediately call splice, this pretty much always means "stay at that index and put something there",
      // so that's what we do.
      // Much easier than forcing the end user to understand such a confusing edge case.
      keepHandleIdx:
        (getHandle, getTracer, Manip.GetRefMap).tupled
          .tapEffect: (handle, tracer, refMap) =>
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
              nodes.size
            )(using
              refMap
            )
        *> keepHandleIdx(
          pass.resultRef.updated(_ => RulesResult.Progress)(
            manip(using on.Ctx(nodes.size))
          )
        )

    def spliceThen(using
        DebugInfo,
        on.Ctx
    )(nodes: Node.Child*)(
        manip: on.Ctx ?=> Rules
    ): Rules =
      spliceThen(nodes)(manip)

    def spliceThen(using
        DebugInfo,
        on.Ctx
    )(nodes: IterableOnce[Node.Child])(
        manip: on.Ctx ?=> Rules
    ): Rules =
      spliceThen(nodes.iterator.toArray)(manip)

    def splice(using
        DebugInfo.Ctx
    )(using
        onCtx: on.Ctx,
        passCtx: pass.Ctx
    )(
        nodes: Iterable[Node.Child]
    ): Rules =
      spliceThen(nodes):
        // If we _now_ have a match count of 0, it means the splice deleted our match.
        // In that case, it's safe to retry at same position because we changed something.
        // Normally that's bad, because it means we matched nothing and will retry in same position.
        if summon[on.Ctx].matchedCount == 0
        then continuePass
        else skipMatch

    def splice(using DebugInfo, on.Ctx, pass.Ctx)(nodes: Node.Child*): Rules =
      splice(nodes)

    def splice(using
        DebugInfo,
        on.Ctx,
        pass.Ctx
    )(
        nodes: IterableOnce[Node.Child]
    ): Rules =
      splice(nodes.iterator.toArray)

    def continuePass(using passCtx: pass.Ctx): Rules =
      passCtx.loop

    def continuePassAtNextNode(using DebugInfo)(using pass.Ctx): Rules =
      atNextNode(continuePass)

    def atNextNode(using
        DebugInfo
    )(using passCtx: pass.Ctx)(manip: Rules): Rules =
      passCtx.strategy.atNext(manip)

    def endPass(using DebugInfo): Rules =
      pass.resultRef.get

    def skipMatch(using
        DebugInfo.Ctx
    )(using onCtx: on.Ctx, passCtx: pass.Ctx): Rules =
      require(
        onCtx.matchedCount > 0,
        s"must have matched at least one node to skip. Matched ${onCtx.matchedCount}"
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
        monoidK.combineK(lhs, rhs)
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
        wrapFn: Manip[Unit] => Manip[Unit] = identity
    ):
      def rules(using DebugInfo.Ctx)(rules: pass.Ctx ?=> Rules): Manip[Unit] =
        val before = getTracer
          .product(Manip.GetRefMap)
          .flatMap: (tracer, refMap) =>
            effect(tracer.beforePass(summon[DebugInfo])(using refMap))
        val after = getTracer
          .product(Manip.GetRefMap)
          .flatMap: (tracer, refMap) =>
            effect(tracer.afterPass(summon[DebugInfo])(using refMap))

        // TODO: consider optimizing the ancestor check if it becomes a bottleneck
        def exceptInError[T](manip: Manip[T])(using DebugInfo.Ctx): Manip[T] =
          import SeqPattern.ops.*
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
          wrapFn = manip => ref.init(init)(defer(wrapFn(manip)))
        )
    end pass

    object pass:
      object resultRef extends Ref[RulesResult]

      final case class Ctx(
          strategy: TraversalStrategy,
          loop: Manip[RulesResult]
      )(using DebugInfo.Ctx):
        lazy val loopAtNext: Manip[RulesResult] =
          strategy.atNext(loop)

      trait TraversalStrategy:
        def atInit(manip: Manip[RulesResult])(using
            DebugInfo.Ctx
        ): Manip[RulesResult]
        def atNext(manip: Manip[RulesResult])(using
            DebugInfo.Ctx
        ): Manip[RulesResult]

      object topDown extends TraversalStrategy:
        def atInit(manip: Manip[RulesResult])(using
            DebugInfo.Ctx
        ): Manip[RulesResult] = manip

        def atNext(manip: Manip[RulesResult])(using
            DebugInfo.Ctx
        ): Manip[RulesResult] =
          val next = commit(manip)
          commit:
            atFirstChild(next)
              | atRightSibling(next)
              | (on(SeqPattern.ops.atEnd).check
                *> atParent(
                  commit(
                    // going right finds either real sibling or sentinel, unless at top
                    atRightSibling(next)
                      | on(SeqPattern.ops.theTop).check *> endPass
                  )
                ))

      object bottomUp extends TraversalStrategy:
        def atInit(manip: Manip[RulesResult])(using
            DebugInfo.Ctx
        ): Manip[RulesResult] =
          lazy val impl: Manip[RulesResult] =
            commit:
              atFirstChild(defer(impl))
                | manip

          impl

        def atNext(manip: Manip[RulesResult])(using
            DebugInfo.Ctx
        ): Manip[RulesResult] =
          val next = commit(manip)
          def atNextCousin[T](manip: Manip[T]): Manip[T] =
            lazy val impl: Manip[T] =
              atFirstChild(manip)
                | atRightSibling(defer(impl))

            atParent(atRightSibling(impl))

          commit:
            // same layer, same parent (next case ensures we already processed any children)
            atRightSibling(next)
            // go all the way into next subtree on the right
              | atNextCousin(atInit(next))
              // up one layer, far left, knowing we looked at all reachable children
              | atParent(atFirstSibling(next))
              // special case: parent is top
              | atParent(on(SeqPattern.ops.theTop).check *> next)
              // onward from top --> done
              | on(SeqPattern.ops.theTop).check *> endPass
  end ops
end Manip
