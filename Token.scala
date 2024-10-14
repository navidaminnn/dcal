package distcompiler

import scala.collection.mutable

trait Token extends Equals, Named:
  require(!Token._nameSet.contains(name), s"duplicate name $name")
  Token._nameSet += name

  final def mkNode(childrenInit: IterableOnce[Node.Child] = Nil): Node =
    Node(this)(childrenInit)

  final def mkNode(childrenInit: Node.Child*): Node =
    Node(this)(childrenInit)

  override def canEqual(that: Any): Boolean =
    that.isInstanceOf[Token]

  override def equals(that: Any): Boolean =
    this eq that.asInstanceOf[AnyRef]

  override def hashCode(): Int =
    System.identityHashCode(this)

  override def toString(): String =
    s"Token(@${hashCode()} $name)"

  final def canBeLookedUp: Boolean = !lookedUpBy.isBacktrack

  def symbolTableFor: Set[Token] = Set.empty
  def lookedUpBy: Pattern[Set[Node]] = Pattern.reject
  def showSource: Boolean = false
end Token

object Token:
  private val _nameSet: mutable.HashSet[String] = mutable.HashSet.empty

  trait ShowSource extends Token:
    override def showSource: Boolean = true

  private var _freshCounter: Long = 0
  private def incFreshCounter(): Long =
    val result = _freshCounter
    _freshCounter += 1
    result

  abstract class Fresh
      extends Token,
        Named(using Named.OwnName(List(s"$$${{ incFreshCounter() }}")))

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
