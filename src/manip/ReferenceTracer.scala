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

import java.io.{ByteArrayOutputStream, PrintStream}

import cats.StackSafeMonad

import forja.*
import forja.dsl.*

import scala.collection.mutable

final class ReferenceTracer(manip: Manip[?])(using
    baseDebugInfo: DebugInfo,
) extends Tracer:
  import ReferenceTracer.*
  private val referenceActionSrc: () => Action =
    referenceEval(manip, baseDebugInfo)

  private var referenceHasTerminated = false
  private val actionsSoFar = mutable.ListBuffer[Action]()

  private def reportErr(action: Action, expectedAction: Action): Nothing =
    val tmpDir = os.pwd / ".ref_compare"
    os.makeDir.all(tmpDir)
    val outBuf = StringBuilder()

    def showAction(action: Action): Unit =
      action match
        case Action.Terminate =>
          outBuf ++= "terminate"
        case Action.AfterTerm =>
          outBuf ++= "after end"
        case Action.Assign(ref, value) =>
          outBuf ++= s"assign $ref <-- $value"
        case Action.Del(ref, debugInfo) =>
          outBuf ++= s"del $ref at $debugInfo"
        case Action.Read(ref, value, debugInfo) =>
          outBuf ++= s"read $ref --> $value at $debugInfo"
        case Action.Branch(debugInfo) =>
          outBuf ++= s"branch at $debugInfo"
        case Action.Commit(debugInfo) =>
          outBuf ++= s"commit at $debugInfo"
        case Action.Backtrack(debugInfo) =>
          outBuf ++= s"backtrack at $debugInfo"
        case Action.Fatal(debugInfo) =>
          outBuf ++= s"fatal at $debugInfo"
        case Action.Crash(ex) =>
          outBuf ++= "crash "
          val out = ByteArrayOutputStream()
          ex.printStackTrace(PrintStream(out))
          outBuf ++= out.toString()

    actionsSoFar.foreach: action =>
      showAction(action)
      outBuf += '\n'

    outBuf ++= "!! "
    showAction(action)
    outBuf += '\n'
    outBuf ++= "EE "
    showAction(expectedAction)
    outBuf += '\n'

    val outFile =
      os.temp(dir = tmpDir, contents = outBuf.toString(), deleteOnExit = false)
    throw new AssertionError(
      s"diverged from reference implementation; see $outFile",
    )
  end reportErr

  private def perform(action: Action): Unit =
    if referenceHasTerminated
    then reportErr(action, Action.AfterTerm)

    val expectedAction = referenceActionSrc()

    if expectedAction == Action.Terminate
    then referenceHasTerminated = true
    if expectedAction != action
    then reportErr(action, expectedAction)
    else actionsSoFar += action
  end perform

  def close(): Unit = ()

  def beforePass(debugInfo: DebugInfo): Unit = ()

  def afterPass(debugInfo: DebugInfo): Unit = ()

  def onRead(
      manip: Manip[?],
      ref: Manip.Ref[?],
      value: Any,
      debugInfo: DebugInfo,
  ): Unit =
    perform(Action.Read(ref, ValueWrapper(value), debugInfo))

  def onAssign(manip: Manip[?], ref: Manip.Ref[?], value: Any): Unit =
    perform(Action.Assign(ref, ValueWrapper(value)))

  def onDel(manip: Manip[?], ref: Manip.Ref[?], debugInfo: DebugInfo): Unit =
    perform(Action.Del(ref, debugInfo))

  def onBranch(manip: Manip[?], debugInfo: DebugInfo): Unit =
    perform(Action.Branch(debugInfo))

  def onBacktrack(manip: Manip[?], debugInfo: DebugInfo): Unit =
    perform(Action.Backtrack(debugInfo))

  def onCommit(manip: Manip[?], debugInfo: DebugInfo): Unit =
    perform(Action.Commit(debugInfo))

  def onRewriteMatch(
      debugInfo: DebugInfo,
      parent: Node.Parent,
      idx: Int,
      matchCount: Int,
  ): Unit = ()

  def onRewriteComplete(
      debugInfo: DebugInfo,
      parent: Node.Parent,
      idx: Int,
      resultCount: Int,
  ): Unit = ()

  def onFatal(manip: Manip[?], debugInfo: DebugInfo, from: DebugInfo): Unit =
    perform(Action.Fatal(debugInfo))
end ReferenceTracer

object ReferenceTracer:
  private class ValueWrapper(target: Any):
    val value = target match
      case _: mutable.Builder[?, ?] => null
      case _                        => target
    override def equals(that: Any): Boolean =
      that match
        case that: ValueWrapper => value == that.value
        case _                  => false

    override def hashCode(): Int =
      value match
        case null  => -1
        case value => value.hashCode()

    override def toString(): String =
      value match
        case null  => "null"
        case value => value.toString()

  private enum Action:
    case Terminate
    case AfterTerm
    case Assign(ref: Manip.Ref[?], value: ValueWrapper)
    case Read(ref: Manip.Ref[?], value: ValueWrapper, debugInfo: DebugInfo)
    case Del(ref: Manip.Ref[?], debugInfo: DebugInfo)
    case Branch(debugInfo: DebugInfo)
    case Commit(debugInfo: DebugInfo)
    case Backtrack(debugInfo: DebugInfo)
    case Fatal(debugInfo: DebugInfo)
    case Crash(throwable: Throwable)

  private final class BacktrackException(val debugInfo: DebugInfo)
      extends RuntimeException(null, null, true, false)
  private final class ExitException(cause: Throwable | Null)
      extends RuntimeException(cause):
    def this() =
      this(null)

  private enum Result[+T]:
    case Backtrack(posInfo: DebugInfo)
    case Resumable(action: Action, resume: () => Result[T])
    case Value(value: T)

    def map[U](fn: T => U): Result[U] =
      flatMap(v => Value(fn(v)))

    def flatMap[U](fn: T => Result[U]): Result[U] =
      this match
        case Backtrack(posInfo) => Backtrack(posInfo)
        case Resumable(action, resume) =>
          Resumable(action, () => resume().flatMap(fn))
        case Value(value) =>
          fn(value)

    def always(fn: => () => Unit): Result[T] =
      this match
        case bt @ Backtrack(_) =>
          fn()
          bt
        case Resumable(action, resume) =>
          Resumable(action, () => resume().always(fn))
        case v @ Value(_) =>
          fn()
          v
      this

    def recover[U >: T](fn: DebugInfo => Result[U]): Result[U] =
      this match
        case Backtrack(posInfo) => fn(posInfo)
        case Resumable(action, resume) =>
          Resumable(action, () => resume().recover(fn))
        case Value(value) => Value(value)

    def run(): () => Action =
      var trampoline: () => (Result[?] | Null) = () => this
      () =>
        trampoline() match
          case null =>
            throw RuntimeException(
              "tried to get actions from exhausted reference impl",
            )
          case Backtrack(posInfo) =>
            throw RuntimeException(s"unrecovered backtrack $posInfo")
          case Resumable(action, resume) =>
            trampoline = resume
            action
          case Value(value) =>
            trampoline = () => null
            Action.Terminate
    end run

  private object Result:
    export monad.pure

    // Note: stack safe _assuming we log regularly_
    given monad: StackSafeMonad[Result] with
      def pure[A](x: A): Result[A] = Result.Value(x)
      def flatMap[A, B](fa: Result[A])(f: A => Result[B]): Result[B] =
        fa.flatMap(f)

  /* FIXME: allow yielding actions without constructing a giant object tree,
   * then we'll be acceptably efficient */

  private def referenceEval(
      manip: Manip[?],
      baseDebugInfo: DebugInfo,
  ): () => Action =
    import cats.syntax.all.given
    import Manip.*

    def perform(action: Action): Result[Unit] =
      Result.Resumable(action, () => Result.pure(()))

    type BacktrackFn = DebugInfo => Result[Nothing]

    def backtrackFatal(
        debugInfo: DebugInfo,
    )(posInfo: DebugInfo): Result[Nothing] =
      for
        _ <- perform(Action.Fatal(debugInfo))
        result <- Result.pure(???)
      yield result

    def impl[T](self: Manip[T])(using refMap: Manip.RefMap): Result[T] =
      def doBacktrack(debugInfo: DebugInfo): Result[T] =
        perform(Action.Backtrack(debugInfo))
          *> Result.Backtrack(debugInfo)

      self match
        case Backtrack(debugInfo) =>
          doBacktrack(debugInfo)
        case Pure(value) =>
          Result.pure(value)
        case Ap(ff, fa) =>
          for
            ffVal <- impl(ff)
            faVal <- impl(fa)
          yield ffVal(faVal)
        case MapOpt(manip, fn) =>
          impl(manip).map(fn)
        case FlatMap(manip, fn) =>
          for
            value <- impl(manip)
            result <- impl(fn(value))
          yield result
        case Restrict(manip, restriction, debugInfo) =>
          impl(manip).flatMap:
            case restriction(value) => Result.pure(value)
            case badVal             => doBacktrack(debugInfo)
        case Effect(fn) =>
          Result.pure(fn())
        case Finally(manip, fn) =>
          impl(manip).always(fn)
        case KeepLeft(left, right) =>
          impl(left).flatMap: lVal =>
            impl(right).map(_ => lVal)
        case KeepRight(left, right) =>
          impl(left).flatMap: _ =>
            impl(right)
        case Commit(manip, debugInfo) =>
          perform(Action.Commit(debugInfo))
            *> impl(manip).recover(backtrackFatal(debugInfo))
        case refInit: RefInit[u, t] =>
          refMap.get(refInit.ref) match
            case Some(_) =>
              doBacktrack(refInit.debugInfo)
            case None =>
              var value = refInit.initFn()
              // On first node init, we are being given a ref to a fresh node.
              // Clone it so we don't step all over the "real one's" work.
              if refInit.ref == Handle.ref
              then
                value.asInstanceOf[Handle] match
                  case Handle.AtTop(top) =>
                    value = Handle.AtTop(top.clone()).asInstanceOf[u]
                  case Handle.AtChild(parent, idx, child) =>
                    val pClone = parent.clone()
                    value = Handle
                      .AtChild(pClone, idx, pClone.children(idx))
                      .asInstanceOf[u]
                  case Handle.Sentinel(parent, idx) =>
                    val pClone = parent.clone()
                    value = Handle.Sentinel(pClone, idx).asInstanceOf[u]

              for
                _ <- perform(Action.Assign(refInit.ref, ValueWrapper(value)))
                result <- impl(refInit.manip)(using
                  refMap.updated(refInit.ref, value),
                )
                _ <- perform(Action.Del(refInit.ref, refInit.debugInfo))
              yield result
        case RefReset(ref, manip, debugInfo) =>
          for
            nextRefMap <- refMap.get(ref) match
              case None => Result.pure(refMap)
              case Some(_) =>
                perform(Action.Del(ref, debugInfo))
                  *> Result.pure(refMap.removed(ref))
            result <- impl(manip)(using nextRefMap)
          yield result
        case RefGet(ref, debugInfo) =>
          refMap.get(ref) match
            case None => doBacktrack(debugInfo)
            case Some(value) =>
              perform(Action.Read(ref, ValueWrapper(value), debugInfo))
                *> Result.pure(value)
        case RefUpdated(ref, fn, manip, debugInfo) =>
          refMap.get(ref) match
            case None => doBacktrack(debugInfo)
            case Some(oldValue) =>
              for
                _ <- perform(
                  Action.Read(ref, ValueWrapper(oldValue), debugInfo),
                )
                value = fn(oldValue)
                _ <- perform(Action.Assign(ref, ValueWrapper(value)))
                result <- impl(manip)(using refMap.updated(ref, value))
              yield result
        case GetTracer => Result.pure(NopTracer)
        case Disjunction(first, second, debugInfo) =>
          perform(Action.Branch(debugInfo))
          *> impl(first).recover: _ =>
            impl(second)
        case Deferred(fn) =>
          impl(fn())
        case TapEffect(manip, fn) =>
          for
            value <- impl(manip)
            _ = fn(value)
          yield value
        case RestrictHandle(fn, manip, debugInfo) =>
          refMap.get(Handle.ref) match
            case None => doBacktrack(debugInfo)
            case Some(oldHandle) =>
              perform(
                Action
                  .Read(Handle.ref, ValueWrapper(oldHandle), debugInfo),
              )
                *> (oldHandle match
                  case fn(handle) =>
                    perform(
                      Action.Assign(Handle.ref, ValueWrapper(handle)),
                    )
                      *> impl(manip)(using
                        refMap.updated(Handle.ref, handle),
                      )
                  case _ => doBacktrack(debugInfo))
    end impl

    impl(manip)(using RefMap.empty)
      .recover(backtrackFatal(baseDebugInfo))
      .run()
end ReferenceTracer
