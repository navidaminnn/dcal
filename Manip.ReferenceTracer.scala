package distcompiler

import distcompiler.dsl.*
import distcompiler.Manip.RefMap
import scala.collection.mutable

import java.util.concurrent.Exchanger
import java.util.concurrent.TimeoutException
import java.time.Duration
import java.util.concurrent.TimeUnit
import java.util.concurrent.SynchronousQueue
import java.io.PrintStream
import java.io.StringBufferInputStream
import java.io.ByteArrayOutputStream

final class ManipReferenceTracer(manip: Manip[?])(using baseDebugInfo: DebugInfo) extends Manip.Tracer:
  import ManipReferenceTracer.*
  private val actionQueue = SynchronousQueue[Action]()
  private val pushQueue = SynchronousQueue[Unit]()

  private val expectedThread = Thread: () =>
    referenceEval(manip, baseDebugInfo)(using ReferenceEvalCtx(pushQueue, actionQueue))
  expectedThread.start()

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

    val outFile = os.temp(dir = tmpDir, contents = outBuf.toString(), deleteOnExit = false)
    throw new AssertionError(s"diverged from reference implementation; see $outFile")

  private def perform(action: Action): Unit =
    if referenceHasTerminated
    then reportErr(action, Action.AfterTerm)

    pushQueue.put(())
    val expectedAction = actionQueue.take()

    if expectedAction == Action.Terminate
    then referenceHasTerminated = true
    if expectedAction != action
    then
      expectedThread.interrupt()
      reportErr(action, expectedAction)
    else actionsSoFar += action
  end perform

  def close(): Unit =
    expectedThread.interrupt()
    expectedThread.join()

  def beforePass(debugInfo: DebugInfo)(using RefMap): Unit = ()

  def afterPass(debugInfo: DebugInfo)(using RefMap): Unit = ()

  def onRead(manip: Manip[?], ref: Manip.Ref[?], value: Any, debugInfo: DebugInfo)(using RefMap): Unit =
    perform(Action.Read(ref, ValueWrapper(value), debugInfo))

  def onAssign(manip: Manip[?], ref: Manip.Ref[?], value: Any)(using RefMap): Unit =
    perform(Action.Assign(ref, ValueWrapper(value)))

  def onDel(manip: Manip[?], ref: Manip.Ref[?], debugInfo: DebugInfo)(using RefMap): Unit =
    perform(Action.Del(ref, debugInfo))

  def onBranch(manip: Manip[?], debugInfo: DebugInfo)(using RefMap): Unit =
    perform(Action.Branch(debugInfo))

  def onBacktrack(manip: Manip[?], debugInfo: DebugInfo)(using RefMap): Unit =
    perform(Action.Backtrack(debugInfo))

  def onCommit(manip: Manip[?], debugInfo: DebugInfo)(using RefMap): Unit =
    perform(Action.Commit(debugInfo))

  def onRewriteMatch(debugInfo: DebugInfo, parent: Node.Parent, idx: Int, matchCount: Int)(using RefMap): Unit = ()

  def onRewriteComplete(debugInfo: DebugInfo, parent: Node.Parent, idx: Int, resultCount: Int)(using RefMap): Unit = ()

  def onFatal(manip: Manip[?], debugInfo: DebugInfo, from: DebugInfo)(using RefMap): Unit =
    perform(Action.Fatal(debugInfo))
end ManipReferenceTracer

object ManipReferenceTracer:
  private class ValueWrapper(target: Any):
    val value = target match
      case _: mutable.Builder[?, ?] => null
      case _ => target
    override def equals(that: Any): Boolean =
      that match
        case that: ValueWrapper => value == that.value
        case _ => false

    override def hashCode(): Int =
      value match
        case null => -1
        case value => value.hashCode()

    override def toString(): String =
      value match
        case null => "null"
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

  private final class BacktrackException(val debugInfo: DebugInfo) extends RuntimeException(null, null, true, false)
  private final class ExitException(cause: Throwable | Null) extends RuntimeException(cause):
    def this() =
      this(null)

  private final class ReferenceEvalCtx(val pushQueue: SynchronousQueue[Unit], val actionQueue: SynchronousQueue[Action])

  private def referenceEval(manip: Manip[?], baseDebugInfo: DebugInfo)(using ctx: ReferenceEvalCtx): Unit =
    import Manip.*

    def drainPush(): Unit =
      ctx.pushQueue.take()
    def perform(action: Action): Unit =
      ctx.actionQueue.put(action)
      drainPush()

    def impl[T](self: Manip[T])(using refMap: Manip.RefMap): T =
      def doBacktrack(debugInfo: DebugInfo): Nothing =
        perform(Action.Backtrack(debugInfo))
        throw BacktrackException(debugInfo)

      self match
        case Backtrack(debugInfo) =>
          doBacktrack(debugInfo)
        case Pure(value) => value
        case Ap(ff, fa) =>
          val ffVal = impl(ff)
          val faVal = impl(fa)
          ffVal(faVal)
        case MapOpt(manip, fn) =>
          fn(impl(manip))
        case FlatMap(manip, fn) =>
          impl(fn(impl(manip)))
        case Restrict(manip, restriction, debugInfo) =>
          impl(manip) match
            case restriction(value) => value
            case badVal => doBacktrack(debugInfo)
        case Effect(fn) =>
          fn()
        case Finally(manip, fn) =>
          try impl(manip)
          finally fn()
        case KeepLeft(left, right) =>
          val lVal = impl(left)
          impl(right)
          lVal
        case KeepRight(left, right) =>
          impl(left)
          impl(right)
        case Commit(manip, debugInfo) =>
          perform(Action.Commit(debugInfo))
          try impl(manip)
          catch case ex: BacktrackException =>
            perform(Action.Fatal(debugInfo))
            throw ExitException(ex)
        case refInit: RefInit[u, t] =>
          refMap.get(refInit.ref) match
            case Some(_) =>
              doBacktrack(refInit.debugInfo)
            case None =>
              var value = refInit.initFn()
              // On first node init, we are being given a ref to a fresh node.
              // Clone it so we don't step all over the "real one's" work.
              if refInit.ref == Manip.Handle.ref
              then value.asInstanceOf[Manip.Handle] match
                case Handle.AtTop(top) =>
                  value = Handle.AtTop(top.clone()).asInstanceOf[u]
                case Handle.AtChild(parent, idx, child) =>
                  val pClone = parent.clone()
                  value = Handle.AtChild(pClone, idx, pClone.children(idx)).asInstanceOf[u]
                case Handle.Sentinel(parent, idx) =>
                  val pClone = parent.clone()
                  value = Handle.Sentinel(pClone, idx).asInstanceOf[u]

              perform(Action.Assign(refInit.ref, ValueWrapper(value)))
              val result = impl(refInit.manip)(using refMap.updated(refInit.ref, value))
              perform(Action.Del(refInit.ref, refInit.debugInfo))
              result
        case RefReset(ref, manip, debugInfo) =>
          val nextRefMap = refMap.get(ref) match
            case None => refMap
            case Some(_) =>
              perform(Action.Del(ref, debugInfo))
              refMap.removed(ref)
          impl(manip)(using nextRefMap)
        case RefGet(ref, debugInfo) =>
          refMap.get(ref) match
            case None => doBacktrack(debugInfo)
            case Some(value) =>
              perform(Action.Read(ref, ValueWrapper(value), debugInfo))
              value
        case RefUpdated(ref, fn, manip, debugInfo) =>
          refMap.get(ref) match
            case None => doBacktrack(debugInfo)
            case Some(oldValue) =>
              perform(Action.Read(ref, ValueWrapper(oldValue), debugInfo))
              val value = fn(oldValue)
              perform(Action.Assign(ref, ValueWrapper(value)))
              impl(manip)(using refMap.updated(ref, value))
        case GetRefMap => refMap
        case GetTracer => Manip.NopTracer
        case Disjunction(first, second, debugInfo) =>
          perform(Action.Branch(debugInfo))
          try impl(first)
          catch case ex: BacktrackException =>
            impl(second)
        case Deferred(fn) =>
          impl(fn())
        case TapEffect(manip, fn) =>
          val value = impl(manip)
          fn(value)
          value
        case RestrictHandle(fn, manip, debugInfo) =>
          refMap.get(Manip.Handle.ref) match
            case None => doBacktrack(debugInfo)
            case Some(oldHandle) =>
              perform(Action.Read(Manip.Handle.ref, ValueWrapper(oldHandle), debugInfo))
              oldHandle match
                case fn(handle) =>
                  perform(Action.Assign(Manip.Handle.ref, ValueWrapper(handle)))
                  impl(manip)(using refMap.updated(Manip.Handle.ref, handle))
                case _ => doBacktrack(debugInfo)
    end impl

    try
      try
        drainPush()
        impl(manip)(using RefMap.empty)
      catch
        case ex: BacktrackException =>
          perform(Action.Fatal(baseDebugInfo))
        case ex: RuntimeException =>
          perform(Action.Crash(ex))
    catch
      case _: InterruptedException =>
        // this is a request to end reference eval
end ManipReferenceTracer
