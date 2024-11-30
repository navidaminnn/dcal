// Copyright 2025 DCal Team
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

import java.nio.channels.ServerSocketChannel
import java.net.InetSocketAddress
import java.nio.channels.SocketChannel
import java.nio.channels.ClosedChannelException
import java.nio.channels.AsynchronousCloseException

import scala.collection.mutable
import java.nio.ByteBuffer
import java.nio.charset.StandardCharsets
import java.nio.CharBuffer
import java.nio.channels.Channels
import java.io.Closeable
import distcompiler.Manip.Handle

final class DebugAdapter(host: String, port: Int) extends Manip.Tracer:
  import DebugAdapter.{EvalState, EvalTag, StackEntry, SourceRecord}
  private val io = DebugAdapter.IO(host, port)

  @scala.annotation.tailrec
  private def handlePointOfInterest(
      debugInfo: DebugInfo,
      refMap: Manip.RefMap,
      evalTag: DebugAdapter.EvalTag
  ): Unit =
    io.witnessSources(debugInfo)
    val shouldWait =
      io.withState: state =>
        evalTag match
          case EvalTag.Backtrack  =>
          case EvalTag.FatalError =>
          case EvalTag.Commit =>
            state.frameIdCounter += state.stackTrace.size
            state.stackTrace.clear()
          case EvalTag.BeforeRewrite =>
          case EvalTag.AfterRewrite  =>
          case EvalTag.BeforePass    =>
          case EvalTag.AfterPass     =>

        val stackEntry = StackEntry(
          source = SourceRecord(
            fileName = debugInfo.records.head.file
          ),
          tag = evalTag,
          line = debugInfo.records.head.line
        )

        if state.stackTrace.headOption != Some(stackEntry)
        then state.stackTrace += stackEntry

        def enforceRunningState(): Unit =
          state.debugInfo = null
          state.refMap = null
          state.clearNodeRefs()

        def beginPause(): Unit =
          println(s"pausing...")
          state.debugInfo = debugInfo
          state.refMap = refMap
          state.evalState = EvalState.Paused

        state.evalState match
          case EvalState.Running =>
            enforceRunningState()
            false
          case EvalState.Pausing =>
            enforceRunningState()
            beginPause()
            io.witnessPause(evalTag)
            true
          case EvalState.Paused =>
            // println(s"paused...")
            state.debugInfo = debugInfo
            state.refMap = refMap
            true
          case EvalState.Stepping =>
            // println(s"stepping...")
            enforceRunningState()
            state.evalState = EvalState.Pausing
            false
          case EvalState.ContinueNextRewrite =>
            enforceRunningState()
            state.evalState = EvalState.NextRewriteSearch
            false
          case EvalState.NextRewriteSearch =>
            enforceRunningState()
            evalTag match
              case EvalTag.BeforeRewrite =>
                beginPause()
                io.witnessPause(evalTag)
                true
              case _ => false

    if shouldWait
    then
      io.awaitStateUpdate()
      // println("wake up...")
      handlePointOfInterest(debugInfo, refMap, evalTag)

  def close(): Unit =
    io.close()

  def beforePass(debugInfo: DebugInfo)(using Manip.RefMap): Unit =
    handlePointOfInterest(debugInfo, summon[Manip.RefMap], EvalTag.BeforePass)

  def afterPass(debugInfo: DebugInfo)(using Manip.RefMap): Unit =
    handlePointOfInterest(debugInfo, summon[Manip.RefMap], EvalTag.AfterPass)

  def onRewriteMatch(
      debugInfo: DebugInfo,
      parent: Node.Parent,
      idx: Int,
      matchedCount: Int
  )(using Manip.RefMap): Unit =
    handlePointOfInterest(
      debugInfo,
      summon[Manip.RefMap],
      EvalTag.BeforeRewrite
    )

  def onRewriteComplete(
      debugInfo: DebugInfo,
      parent: Node.Parent,
      idx: Int,
      resultCount: Int
  )(using Manip.RefMap): Unit =
    handlePointOfInterest(debugInfo, summon[Manip.RefMap], EvalTag.AfterRewrite)

  def onCommit(debugInfo: DebugInfo)(using Manip.RefMap): Unit =
    handlePointOfInterest(debugInfo, summon[Manip.RefMap], EvalTag.Commit)
  def onBacktrack(debugInfo: DebugInfo)(using Manip.RefMap): Unit =
    handlePointOfInterest(debugInfo, summon[Manip.RefMap], EvalTag.Backtrack)

  def onFatal(debugInfo: DebugInfo, from: DebugInfo)(using Manip.RefMap): Unit =
    handlePointOfInterest(debugInfo, summon[Manip.RefMap], EvalTag.FatalError)

object DebugAdapter:
  final case class SourceRecord(fileName: String):
    def asJSON: ujson.Obj =
      ujson.Obj(
        "name" -> fileName,
        "path" -> fileName
      )

  enum EvalState:
    case Paused, Pausing
    case Running
    case Stepping
    case ContinueNextRewrite, NextRewriteSearch

  enum EvalTag:
    case Backtrack
    case FatalError
    case Commit
    case BeforeRewrite
    case AfterRewrite
    case BeforePass
    case AfterPass

    def desc: String =
      this match
        case Backtrack     => "on backtrack"
        case FatalError    => "on fatal error"
        case Commit        => "on commit"
        case BeforeRewrite => "before rewrite"
        case AfterRewrite  => "after rewrite"
        case BeforePass    => "before pass"
        case AfterPass     => "after pass"

  final case class StackEntry(source: SourceRecord, tag: EvalTag, line: Int)

  final class State:
    var evalState = EvalState.Paused

    var evalTag: EvalTag | Null = null
    var debugInfo: DebugInfo | Null = null
    var refMap: Manip.RefMap | Null = null
    var frameIdCounter = 0

    val knownSources = mutable.HashSet[SourceRecord]()
    val stackTrace = mutable.ArrayBuffer[StackEntry]()

    def clearNodeRefs(): Unit =
      nodeIdx = 1
      nodeById.clear()
      idByNode.clear()

    private var nodeIdx = 1
    private val nodeById = mutable.HashMap[Int, Node.All]()
    private val idByNode = mutable.HashMap[ById[Node.All], Int]()

    def refOf(node: Node.All): Int =
      idByNode.get(ById(node)) match
        case Some(idx) => idx
        case None =>
          val idx = nodeIdx
          nodeIdx += 1
          nodeById(idx) = node
          idByNode(ById(node)) = idx
          idx

    def refOfHandle(handle: Manip.Handle): Int =
      handle match
        case Manip.Handle.AtChild(_, _, child) => refOf(child)
        case Manip.Handle.AtTop(top)           => refOf(top)
        case Manip.Handle.Sentinel(parent, _)  => refOf(parent)

    def getNodeByRef(ref: Int): Option[Node.All] =
      nodeById.get(ref)

  object DAPProtocolMessage:
    def unapply(js: ujson.Value): Option[(Int, String, ujson.Obj)] =
      js match
        case obj: ujson.Obj
            if obj("seq").numOpt
              .exists(_.isValidInt) && obj("type").strOpt.nonEmpty =>
          Some((obj("seq").num.toInt, obj("type").str, obj))
        case _ => None

  object DAPRequest:
    def unapply(js: ujson.Value): Option[(Int, String, ujson.Value)] =
      js match
        case DAPProtocolMessage(seq, "request", obj)
            if obj("command").strOpt.nonEmpty =>
          Some(
            (
              seq,
              obj("command").str,
              obj.obj.getOrElse("arguments", ujson.Obj())
            )
          )
        case _ => None

  final class IO(host: String, port: Int) extends java.io.Closeable:
    io =>

    def witnessSources(debugInfo: DebugInfo): Unit =
      this.connectionOpt match
        case None =>
          withState: state =>
            state.knownSources ++= debugInfo.records.iterator.map(rec =>
              SourceRecord(rec.file)
            )
        case Some(connection) =>
          withState: state =>
            debugInfo.records.iterator.foreach: rec =>
              val record = SourceRecord(rec.file)
              if !state.knownSources(record)
              then
                state.knownSources += record
                connection.sendSource(record)

    def witnessPause(evalTag: EvalTag): Unit =
      this.connectionOpt match
        case None =>
        case Some(connection) =>
          connection.sendEvent(
            event = "stopped",
            body = ujson.Obj(
              "reason" -> "step",
              "description" -> s"Paused ${evalTag.desc}",
              "threadId" -> 1
            )
          )

    private final class Connection(sock: SocketChannel)
        extends Runnable,
          Closeable:
      conn =>
      private val input = Channels.newInputStream(sock)
      private val output = Channels.newOutputStream(sock)
      private var responseIdx = 0
      private val thread = Thread(this, "debug-adapter-connection")
      thread.start()

      private object state:
        private val headerParts = mutable.ListBuffer.empty[String]
        private val headerPattern = raw"""Content-Length: (\d+)""".r
        private val builder = StringBuilder()

        def onChar(char: Char): Unit =
          (builder.lastOption, char) match
            case (Some('\r'), '\n') =>
              builder.deleteCharAt(builder.size - 1)
              val part = builder.result()
              builder.clear()
              if part == ""
              then onHeader()
              else headerParts += part
            case (_, ch) => builder += ch

        def onHeader(): Unit =
          var contentLength = -1
          headerParts.foreach:
            case headerPattern(contentLengthStr) =>
              contentLength = contentLengthStr.toInt
            case part =>
              throw AssertionError(s"unrecognized DAP header: $part")

          assert(contentLength != -1, "content length missing from DAP header")

          val bytes = input.readNBytes(contentLength)
          handleMsg(ujson.read(bytes))

      def run(): Unit =
        def closedConnection(): Unit =
          connectionOpt = None
          println(s"closed connection")
        while true
        do
          try
            val result = input.read()
            if result == -1
            then
              sock.close()
              closedConnection()
              return

            state.onChar(result.toChar)
          catch
            case _: (ClosedChannelException | AsynchronousCloseException) =>
              // we are closing, goodbye
              closedConnection()
              return

      def sendMsg(payload: ujson.Obj): Unit =
        // synchronized, because close() might call us to send a terminate event
        conn.synchronized:
          payload("seq") = responseIdx
          responseIdx += 1
          // println(s"send $payload")
          val payloadBytes = ujson.writeToByteArray(payload)
          output.write(
            s"Content-Length: ${payloadBytes.length}\r\n\r\n"
              .getBytes(StandardCharsets.UTF_8)
          )
          output.write(payloadBytes)

      def sendResp(
          requestSeq: Int,
          command: String,
          body: ujson.Value = ujson.Null
      ): Unit =
        sendMsg(
          ujson.Obj(
            "request_seq" -> requestSeq,
            "success" -> true,
            "command" -> command,
            "type" -> "response",
            "body" -> body
          )
        )

      def sendEvent(
          event: String,
          body: ujson.Value = ujson.Null
      ): Unit =
        sendMsg(
          ujson.Obj(
            "type" -> "event",
            "event" -> event,
            "body" -> body
          )
        )

      def sendSource(record: SourceRecord): Unit =
        sendEvent(
          event = "loadedSource",
          body = ujson.Obj(
            "reason" -> "new",
            "source" -> record.asJSON
          )
        )

      private def handleMsg(msg: ujson.Value): Unit =
        msg match
          case DAPRequest(seq, "initialize", req) =>
            // println(s"initialize req: $req")
            sendResp(
              requestSeq = seq,
              command = "initialize"
            )
          case DAPRequest(seq, "attach", req) =>
            // println(s"attach req: $req")
            sendResp(
              requestSeq = seq,
              command = "attach"
            )
            sendEvent(
              event = "stopped",
              body = ujson.Obj(
                "reason" -> "pause",
                "description" -> "Ready to start",
                "threadId" -> 1
              )
            )
            withState: state =>
              state.knownSources.foreach(sendSource)
          case DAPRequest(seq, "disconnect", req) =>
            // println(s"disconnect req: $req")
            sendResp(
              requestSeq = seq,
              command = "disconnect"
            )
            sock.close()
          case DAPRequest(seq, "threads", req) =>
            // println(s"threads req: $req")
            sendResp(
              requestSeq = seq,
              command = "threads",
              body = ujson.Obj(
                "threads" -> ujson.Arr(
                  ujson.Obj(
                    "id" -> 1,
                    "name" -> "singleton",
                    "threadId" -> 1
                  )
                )
              )
            )
          case DAPRequest(seq, "pause", req) =>
            // println(s"pause req: $req")
            val wasPaused =
              io.withState: state =>
                val wasPaused = state.evalState match
                  case EvalState.Paused => true
                  case _ =>
                    state.evalState = EvalState.Pausing
                    false

                wasPaused
            sendResp(
              requestSeq = seq,
              command = "pause"
            )
            if wasPaused
            then
              sendEvent(
                event = "stopped",
                body = ujson.Obj(
                  "reason" -> "pause",
                  "description" -> "Paused",
                  "threadId" -> 1
                )
              )
          case DAPRequest(seq, "stackTrace", req) =>
            // println(s"stackTrace req: $req")
            withState: state =>
              state.evalState match
                case EvalState.Paused =>
                  val frames = state.stackTrace.zipWithIndex
                    .map: (rec, idx) =>
                      ujson.Obj(
                        "id" -> (state.frameIdCounter + idx),
                        "name" -> rec.source.fileName,
                        "path" -> rec.source.fileName,
                        "line" -> rec.line,
                        "column" -> 0,
                        "origin" -> "manip",
                        "source" -> rec.source.asJSON
                      )

                  sendResp(
                    requestSeq = seq,
                    command = "stackTrace",
                    body = ujson.Obj(
                      "stackFrames" -> frames.reverseIterator,
                      "totalFrames" -> frames.size
                    )
                  )
                case _ => // TODO: error or something
          case DAPRequest(seq, "continue", req) =>
            // println(s"continue req: $req")
            io.withState: state =>
              state.evalState = EvalState.Running
            sendResp(
              requestSeq = seq,
              command = "continue"
            )
          case DAPRequest(seq, "next", req) =>
            // println(s"next req: $req")
            io.withState: state =>
              state.evalState = EvalState.Stepping
            sendResp(
              requestSeq = seq,
              command = "next"
            )
          case DAPRequest(seq, "stepIn", req) =>
            io.withState: state =>
              state.evalState = EvalState.ContinueNextRewrite
            sendResp(
              requestSeq = seq,
              command = "stepIn"
            )
          case DAPRequest(seq, "scopes", req) =>
            // println(s"scopes req: $req")
            val frameId = req("frameId").num.toInt
            io.withState: state =>
              // val node = state.getNodeByRef(frameId)
              state.refMap.nn.get(Manip.Handle.ref) match
                case None =>
                case Some(Manip.Handle.AtTop(top)) =>
                  sendResp(
                    requestSeq = seq,
                    command = "scopes",
                    body = ujson.Obj(
                      "scopes" -> ujson.Arr(
                        ujson.Obj(
                          "name" -> "Tree (at top)",
                          "variablesReference" -> state.refOf(top),
                          "namedVariables" -> top.children.size
                        )
                      )
                    )
                  )
                case Some(Manip.Handle.AtChild(parent, idx, child)) =>
                  val childName = child match
                    case node: Node           => node.token.name
                    case embed: Node.Embed[?] => s"embed ${embed.meta.tag}"
                  sendResp(
                    requestSeq = seq,
                    command = "scopes",
                    body = ujson.Obj(
                      "scopes" -> ujson.Arr(
                        ujson.Obj(
                          "name" -> s"Tree (at $childName)",
                          "variablesReference" -> state.refOf(child),
                          "namedVariables" -> (child match
                            case parent: Node.Parent => parent.children.size + 1
                            case _                   => 1
                          )
                        )
                      )
                    )
                  )
                case Some(Manip.Handle.Sentinel(parent, idx)) =>
                  val parentName = parent match
                    case top: Node.Top => "top"
                    case node: Node    => node.token.name
                  sendResp(
                    requestSeq = seq,
                    command = "scopes",
                    body = ujson.Obj(
                      "scopes" -> ujson.Arr(
                        ujson.Obj(
                          "name" -> s"Tree (past-the-end $idx of $parentName)",
                          "variablesReference" -> state.refOf(parent),
                          "namedVariables" -> 1
                        )
                      )
                    )
                  )
          case DAPRequest(seq, "variables", req) =>
            // println(s"variables req: $req")
            val ref = req("variablesReference").num.toInt
            io.withState: state =>
              def variableOf(node: Node.All, idxInParent: Int): ujson.Obj =
                node match
                  case node: Node =>
                    val pfx =
                      if idxInParent == -1
                      then s"[${node.idxInParent}]"
                      else s"parent [$idxInParent]"
                    ujson.Obj(
                      "name" -> s"$pfx ${node.token.name}",
                      "value" -> node.sourceRange.decodeString(),
                      "variablesReference" -> state.refOf(node),
                      "namedVariables" -> (node.children.size + 1)
                    )
                  case embed: Node.Embed[?] =>
                    val pfx =
                      if idxInParent == -1
                      then s"[${node.idxInParent}]"
                      else s"parent [$idxInParent]"
                    ujson.Obj(
                      "name" -> s"$pfx ${embed.meta.tag}",
                      "value" -> embed.value.toString(),
                      "variablesReference" -> state.refOf(node),
                      "namedVariables" -> 1
                    )
                  case top: Node.Top =>
                    ujson.Obj(
                      "name" -> (if idxInParent == -1 then "top"
                                 else s"parent [$idxInParent] top"),
                      "value" -> "",
                      "variablesReference" -> state.refOf(top),
                      "namedVariables" -> top.children.size
                    )

              def variablesOf(node: Node.All): ujson.Arr =
                node match
                  case node: Node =>
                    ujson.Arr.from(
                      node.parent.map(variableOf(_, node.idxInParent)).iterator
                        ++ node.children.iterator.map(variableOf(_, -1))
                    )
                  case embed: Node.Embed[?] =>
                    embed.parent.map(variableOf(_, embed.idxInParent))
                  case top: Node.Top =>
                    top.children.iterator.map(variableOf(_, -1))

              state.getNodeByRef(ref) match
                case None =>
                case Some(node: Node.All) =>
                  sendResp(
                    requestSeq = seq,
                    command = "variables",
                    body = ujson.Obj(
                      "variables" -> variablesOf(node)
                    )
                  )
          case _ =>
            println(s"unsupported DAP request ${msg.render()}")

      def close(): Unit =
        sendEvent(
          event = "terminated"
        )
        sock.close()
        thread.join()

    private var connectionOpt: Option[Connection] = None

    private val _state = State()

    def withState[T](fn: State => T): T =
      io.synchronized:
        val result = fn(_state)
        io.notifyAll()
        result

    def awaitStateUpdate(): Unit =
      io.synchronized(io.wait())

    private final class ConnectionAccepter extends Runnable, Closeable:
      private val serverChannel =
        ServerSocketChannel
          .open()
          .setOption(java.net.StandardSocketOptions.SO_REUSEADDR, true)
          .bind(InetSocketAddress(host, port))
      private val thread = Thread(this, "debug-adapter-listener")
      thread.start()

      def run(): Unit =
        println(s"listening for DAP on ${serverChannel.getLocalAddress()}")
        while true
        do
          try
            val sock = serverChannel.accept()
            println(s"received connection from ${sock.getRemoteAddress()}")
            io.synchronized:
              connectionOpt match
                case None =>
                  connectionOpt = Some(Connection(sock))
                case Some(connection) =>
                  connectionOpt = None
                  connection.close()
                  sock.close()
          catch
            case _: (ClosedChannelException | AsynchronousCloseException) =>
              // means we are closing
              return

      def close(): Unit =
        serverChannel.close()
        thread.join()

    private val connectionAccepter = ConnectionAccepter()

    def close(): Unit =
      io.synchronized(connectionAccepter.close())
      io.synchronized:
        connectionOpt match
          case None =>
          case Some(connection) =>
            connection.close()
            connectionOpt = None
