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

import com.github.difflib.{DiffUtils, UnifiedDiffUtils}
import scala.jdk.CollectionConverters.*

import forja.*
import forja.wf.Wellformed
import forja.util.{toShortString, ++}

import Manip.Ref

trait Tracer extends java.io.Closeable:
  def beforePass(debugInfo: DebugInfo): Unit
  def afterPass(debugInfo: DebugInfo): Unit
  def onRead(
      manip: Manip[?],
      ref: Ref[?],
      value: Any,
      debugInfo: DebugInfo,
  ): Unit
  def onAssign(manip: Manip[?], ref: Ref[?], value: Any): Unit
  def onDel(manip: Manip[?], ref: Ref[?], debugInfo: DebugInfo): Unit
  def onBranch(manip: Manip[?], debugInfo: DebugInfo): Unit
  def onCommit(manip: Manip[?], debugInfo: DebugInfo): Unit
  def onBacktrack(manip: Manip[?], debugInfo: DebugInfo): Unit
  def onFatal(manip: Manip[?], debugInfo: DebugInfo, from: DebugInfo): Unit
  def onRewriteMatch(
      debugInfo: DebugInfo,
      parent: Node.Parent,
      idx: Int,
      matchedCount: Int,
  ): Unit
  def onRewriteComplete(
      debugInfo: DebugInfo,
      parent: Node.Parent,
      idx: Int,
      resultCount: Int,
  ): Unit

abstract class AbstractNopTracer extends Tracer:
  def beforePass(debugInfo: DebugInfo): Unit = ()
  def afterPass(debugInfo: DebugInfo): Unit = ()
  def onRead(
      manip: Manip[?],
      ref: Ref[?],
      value: Any,
      debugInfo: DebugInfo,
  ): Unit = ()
  def onAssign(manip: Manip[?], ref: Ref[?], value: Any): Unit =
    ()
  def onDel(manip: Manip[?], ref: Ref[?], debugInfo: DebugInfo): Unit = ()
  def onBranch(manip: Manip[?], debugInfo: DebugInfo): Unit = ()
  def onCommit(manip: Manip[?], debugInfo: DebugInfo): Unit = ()
  def onBacktrack(manip: Manip[?], debugInfo: DebugInfo): Unit =
    ()
  def onFatal(manip: Manip[?], debugInfo: DebugInfo, from: DebugInfo): Unit =
    ()
  def onRewriteMatch(
      debugInfo: DebugInfo,
      parent: Node.Parent,
      idx: Int,
      matchedCount: Int,
  ): Unit = ()
  def onRewriteComplete(
      debugInfo: DebugInfo,
      parent: Node.Parent,
      idx: Int,
      resultCount: Int,
  ): Unit = ()
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
  private var currHandle: Option[Handle] = None

  private def logln(str: String): Unit =
    out.write(str.getBytes())
    out.write('\n')
    out.flush()

    lineCount += 1

    if limit != -1 && lineCount >= limit
    then throw RuntimeException(s"logged $lineCount lines")

  private def treeDesc: geny.Writable =
    currHandle match
      case None => "<no tree>"
      case Some(handle) =>
        handle.findTop match
          case None => "<??? top not found ???>"
          case Some(top) =>
            top.toPrettyWritable(Wellformed.empty)

  def beforePass(debugInfo: DebugInfo): Unit =
    logln(s"pass init $debugInfo at $treeDesc")

  def afterPass(debugInfo: DebugInfo): Unit =
    logln(s"pass end $debugInfo at $treeDesc")

  def onRead(
      manip: Manip[?],
      ref: Ref[?],
      value: Any,
      debugInfo: DebugInfo,
  ): Unit =
    value match
      case value: AnyVal =>
        logln(s"read $ref --> $value at $treeDesc")
      case value: AnyRef =>
        logln(s"read $ref --> ${value.toShortString()} at $treeDesc")

  def onAssign(manip: Manip[?], ref: Ref[?], value: Any): Unit =
    value match
      case value: AnyVal =>
        logln(s"assign $ref <-- ${value} at $treeDesc")
      case value: AnyRef =>
        logln(s"assign $ref <-- ${value.toShortString} at $treeDesc")

  def onDel(manip: Manip[?], ref: Ref[?], debugInfo: DebugInfo): Unit =
    logln(s"del $ref $debugInfo")

  def onBranch(manip: Manip[?], debugInfo: DebugInfo): Unit =
    logln(s"branch $debugInfo at $treeDesc")

  def onCommit(manip: Manip[?], debugInfo: DebugInfo): Unit =
    logln(s"commit $debugInfo at $treeDesc")

  def onBacktrack(manip: Manip[?], debugInfo: DebugInfo): Unit =
    logln(s"backtrack $debugInfo at $treeDesc")

  def onFatal(manip: Manip[?], debugInfo: DebugInfo, from: DebugInfo): Unit =
    logln(s"fatal $debugInfo from $from at $treeDesc")

  def onRewriteMatch(
      debugInfo: DebugInfo,
      parent: Node.Parent,
      idx: Int,
      matchedCount: Int,
  ): Unit =
    logln(
      s"rw match $debugInfo, from $idx, $matchedCount nodes in parent $parent",
    )

  def onRewriteComplete(
      debugInfo: DebugInfo,
      parent: Node.Parent,
      idx: Int,
      resultCount: Int,
  ): Unit =
    logln(
      s"rw done $debugInfo, from $idx, $resultCount nodes in parent $parent",
    )

final class RewriteDebugTracer(debugFolder: os.Path) extends AbstractNopTracer:
  os.remove.all(debugFolder) // no confusing left-over files!
  os.makeDir.all(debugFolder)
  private var passCounter = 0
  private var rewriteCounter = 0

  private var currHandle: Option[Handle] = None

  private def treeDesc: geny.Writable =
    currHandle match
      case None => "<no tree>"
      case Some(handle) =>
        handle.findTop match
          case None => "<??? top not found ???>"
          case Some(top) =>
            top.toPrettyWritable(Wellformed.empty)

  private def pathAt(
      passIdx: Int,
      rewriteIdx: Int,
      isDiff: Boolean = false,
  ): os.Path =
    val ext = if isDiff then ".diff" else ".txt"
    debugFolder / f"$passIdx%03d" / f"$rewriteIdx%03d$ext%s"

  private def currPath: os.Path =
    pathAt(passCounter, rewriteCounter)

  private def prevPath: os.Path =
    pathAt(passCounter, rewriteCounter - 1)

  private def diffPath: os.Path =
    pathAt(passCounter, rewriteCounter, isDiff = true)

  private def takeSnapshot(debugInfo: DebugInfo): Unit =
    os.write(
      target = currPath,
      data = (s"//! $debugInfo\n": geny.Writable) ++ treeDesc ++ "\n",
      createFolders = true,
    )

  private def takeDiff(): Unit =
    val prevLines = os.read.lines(prevPath).asJava
    val currLines = os.read.lines(currPath).asJava
    val patch = DiffUtils.diff(prevLines, currLines)
    val unifiedDiff = UnifiedDiffUtils.generateUnifiedDiff(
      prevPath.toString(),
      currPath.toString(),
      prevLines,
      patch,
      3,
    )

    os.write.over(
      diffPath,
      unifiedDiff.asScala.mkString("\n"),
    )

  override def onAssign(manip: Manip[?], ref: Ref[?], value: Any): Unit =
    if ref == Handle.ref
    then currHandle = Some(value.asInstanceOf[Handle])

  override def onDel(
      manip: Manip[?],
      ref: Ref[?],
      debugInfo: DebugInfo,
  ): Unit =
    if ref == Handle.ref
    then currHandle = None

  override def beforePass(debugInfo: DebugInfo): Unit =
    passCounter += 1
    rewriteCounter = 0
    takeSnapshot(debugInfo)

  override def afterPass(debugInfo: DebugInfo): Unit =
    rewriteCounter += 1
    takeSnapshot(debugInfo)

  override def onRewriteComplete(
      debugInfo: DebugInfo,
      parent: Node.Parent,
      idx: Int,
      resultCount: Int,
  ): Unit =
    rewriteCounter += 1
    takeSnapshot(debugInfo)
    takeDiff()
