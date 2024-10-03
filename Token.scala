package distcompiler

trait Token extends Equals, Named:
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

  final def canBeLookedUp: Boolean = lookedUpBy ne Pattern.Reject

  def symbolTableFor: List[Token] = Nil
  def lookedUpBy: Pattern[Node] = Pattern.Reject
  def showSource: Boolean = false
end Token

object Token:
  extension (token: Token)
    def apply(children: Node.Child*): Node =
      Node(token)(children)
    def apply(children: IterableOnce[Node.Child]): Node =
      Node(token)(children)
    def apply(sourceRange: String): Node =
      Node(token)().at(sourceRange)
    def apply(sourceRange: SourceRange): Node =
      Node(token)().at(sourceRange)

    def unapply(node: Node.All): unapplyImpl =
      node match
        case node: Node if node.token == token =>
          unapplyImpl(node)
        case _ => unapplyImpl(null)

  final class unapplyImpl(val nodeOpt: Node | Null) extends AnyVal:
    def isEmpty: Boolean = nodeOpt eq null
    def get: Node = nodeOpt.nn
end Token
