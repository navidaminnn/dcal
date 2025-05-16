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

import forja.*
import forja.dsl.*
import forja.util.toShortString

enum Handle:
  assertCoherence()

  case AtTop(top: Node.Top)
  case AtChild(parent: Node.Parent, idx: Int, child: Node.Child)
  case Sentinel(parent: Node.Parent, idx: Int)

  def treeDescr: String =
    this match
      case Handle.AtTop(top) => s"top $top"
      case Handle.AtChild(parent, idx, child) =>
        if parent.children.lift(idx).exists(_ eq child)
        then s"child $idx of $parent"
        else s"!!mismatch child $idx of $parent\n!!and $child"
      case Handle.Sentinel(parent, idx) =>
        s"end [idx=$idx] of $parent"

  def assertCoherence(): Unit =
    this match
      case AtTop(_) =>
      case AtChild(parent, idx, child) =>
        if parent.children.isDefinedAt(idx) && (parent.children(idx) eq child)
        then () // ok
        else
          throw NodeError(
            s"mismatch: idx $idx in ${parent.toShortString()} and ptr -> ${child.toShortString()}",
          )
      case Sentinel(parent, idx) =>
        if parent.children.length != idx
        then
          throw NodeError(
            s"mismatch: sentinel at $idx does not point to end ${parent.children.length} of ${parent.toShortString()}",
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
      p.inspect:
        on(theTop).value
          | atAncestor(on(theTop).value)
    this match
      case AtTop(top)            => Some(top)
      case AtChild(parent, _, _) => forParent(parent)
      case Sentinel(parent, _)   => forParent(parent)

object Handle:
  object ref extends Manip.Ref[Handle]

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
