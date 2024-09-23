package distcompiler

trait Token extends Equals, Named:
  final override def canEqual(that: Any): Boolean =
    that.isInstanceOf[Token]

  final override def equals(that: Any): Boolean =
    this eq that.asInstanceOf[AnyRef]

  final override def hashCode(): Int =
    System.identityHashCode(this)

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
end Token
