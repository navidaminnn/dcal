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

package forja

import forja.util.{TokenMapFactory, Named}
import forja.dsl.*
import forja.src.SourceRange
import java.lang.ref.{WeakReference, ReferenceQueue}

trait Token extends Named, TokenMapFactory.Mapped:
  private val sym = Token.TokenSym(nameSegments)

  final override def equals(that: Any): Boolean =
    that match
      case tok: Token => sym == tok.sym
      case _          => false

  final override def hashCode(): Int = sym.hashCode()

  final def mkNode(childrenInit: IterableOnce[Node.Child] = Nil): Node =
    Node(this)(childrenInit)

  final def mkNode(childrenInit: Node.Child*): Node =
    Node(this)(childrenInit)

  override def toString(): String =
    s"Token($name)"

  final def canBeLookedUp: Boolean = !lookedUpBy.isBacktrack

  def symbolTableFor: Set[Token] = Set.empty
  def lookedUpBy: Manip[Set[Node]] = backtrack
  def showSource: Boolean = false
end Token

object Token:
  private final class TokenSym private (val nameSegments: List[String]):
    override def equals(that: Any): Boolean =
      this `eq` that.asInstanceOf[AnyRef]
    override def hashCode(): Int = nameSegments.hashCode()
  end TokenSym

  private object TokenSym:
    private final class TokenSymRef(
        val nameSegments: List[String],
        sym: TokenSym,
    ) extends WeakReference[TokenSym](sym, canonicalRefQueue)

    private val canonicalRefQueue = ReferenceQueue[TokenSym]()
    private val canonicalMap =
      scala.collection.concurrent.TrieMap[List[String], TokenSymRef]()

    private def cleanQueue(): Unit =
      var ref: TokenSymRef | Null = null
      while
        ref = canonicalRefQueue.poll().asInstanceOf[TokenSymRef | Null]
        ref ne null
      do canonicalMap.remove(ref.nameSegments)
      end while

    def apply(nameSegments: List[String]): TokenSym =
      var sym: TokenSym | Null = null
      // if invoked, forms a GC root for our new sym so it won't be reclaimed immediately after construction
      lazy val freshSym = new TokenSym(nameSegments)
      while sym eq null do
        cleanQueue()
        val ref = canonicalMap.getOrElseUpdate(
          nameSegments,
          TokenSymRef(nameSegments, freshSym),
        )
        // we might have just barely sniped a ref that cleanQueue missed. if sym is null, go around again.
        sym = ref.get()
      end while
      sym.nn
  end TokenSym

  trait ShowSource extends Token:
    override def showSource: Boolean = true

  extension (token: Token)
    def apply(children: Node.Child*): Node =
      Node(token)(children)
    def apply(children: IterableOnce[Node.Child]): Node =
      Node(token)(children)
    def apply(sourceRange: String): Node =
      Node(token)().at(sourceRange)
    def apply(sourceRange: SourceRange): Node =
      Node(token)().at(sourceRange)

    def unapplySeq(node: Node): Option[Node.childrenAccessor] =
      if node.token == token
      then Some(Node.childrenAccessor(node.children))
      else None
end Token
