package distcompiler

import cats.Eval
import cats.data.Chain
import cats.syntax.all.given
import scala.collection.mutable
import izumi.reflect.Tag
import distcompiler.Node.Top

final case class NodeError(msg: String) extends RuntimeException(msg)

final class Node(val token: Token)(childrenInit: IterableOnce[Node.Child] = Nil)
    extends Node.Child,
      Node.Sibling,
      Node.Parent(childrenInit),
      Node.Traversable:
  thisNode =>

  override def asNode: Node = this

  override def asNonFloatingParent: Node | Top = this

  override type This = Node
  override def cloneEval(): Eval[Node] =
    Chain
      .traverseViaChain(children.toIndexedSeq)(_.cloneEval())
      .map(clonedChildren => Node(token)(clonedChildren.toIterable))

  private var _sourceRange: SourceRange | Null = null

  def extendLocation(sourceRange: SourceRange): this.type =
    if _sourceRange eq null
    then _sourceRange = sourceRange
    else _sourceRange = _sourceRange.nn <+> sourceRange
    this

  def at(string: String): this.type =
    at(Source.fromString(string))

  def at(source: Source): this.type =
    at(SourceRange.entire(source))

  def at(sourceRange: SourceRange): this.type =
    if _sourceRange eq null
    then
      _sourceRange = sourceRange
      this
    else throw NodeError("node source range already set")

  def like(other: Node): this.type =
    if other._sourceRange eq null
    then this
    else at(other._sourceRange.nn)

  def sourceRange: SourceRange =
    var rangeAcc: SourceRange | Null = null
    traverse:
      case node: Node =>
        if node._sourceRange ne null
        then
          if rangeAcc ne null
          then rangeAcc = rangeAcc.nn <+> node._sourceRange.nn
          else rangeAcc = node._sourceRange
          Node.TraversalAction.SkipChildren
        else Node.TraversalAction.Continue
      case _: Node.Leaf =>
        Node.TraversalAction.SkipChildren

    if rangeAcc ne null
    then rangeAcc.nn
    else SourceRange.entire(Source.empty)

  private var _scopeRelevance: Int =
    if token.canBeLookedUp then 1 else 0

  override def unparent(): this.type =
    if _scopeRelevance > 0
    then parent.decScopeRelevance()

    super.unparent()

  override def ensureParent(parent: Node.Parent, idxInParent: Int): this.type =
    val prevParent = parent
    super.ensureParent(parent, idxInParent)

    // We can ensureParent without unparent() if only the idx changes.
    // In that case, don't inc twice.
    if _scopeRelevance > 0 && (prevParent ne parent)
    then parent.incScopeRelevance()

    this

  def iteratorDescendants: Iterator[Node.Child] =
    Iterator
      .unfold(firstChild):
        case sentinel: Node.RightSiblingSentinel =>
          sentinel.parent match
            case root: Node.Root => None
            case myself if myself eq thisNode =>
              None // don't climb up beyond where we started
            case parentNode: Node =>
              Some((None, parentNode.rightSibling))
        case node: Node => Some((Some(node), node.firstChild))
        case leaf: (Node.Leaf & Node.Child) =>
          Some((Some(leaf), leaf.rightSibling))
      .flatten

  override def iteratorErrors: Iterator[Node] =
    if token == Builtin.Error
    then Iterator.single(thisNode)
    else children.iterator.foldLeft(Iterator.empty[Node])(_ ++ _.iteratorErrors)

  def lookup: List[Node] =
    assert(token.canBeLookedUp)
    parent.findNodeByKey(thisNode)

  def lookupRelativeTo(referencePoint: Node): List[Node] =
    assert(token.canBeLookedUp)
    referencePoint.findNodeByKey(thisNode)

end Node

object Node:
  enum TraversalAction:
    case SkipChildren, Continue
  end TraversalAction

  sealed trait Traversable:
    thisTraversable =>

    final def traverse(fn: Node.Child => Node.TraversalAction): Unit =
      @scala.annotation.tailrec
      def impl(traversable: Node.Sibling): Unit =
        traversable match
          case sentinel: Node.RightSiblingSentinel =>
            sentinel.parent match
              case _: Node.Root => () // done
              case parentNode: Node if parentNode eq thisTraversable =>
                () // we found ourselves again, stop before we go "too far up"
              case parentNode: Node =>
                impl(parentNode.rightSibling)

          case node: Node =>
            import TraversalAction.*
            fn(node) match
              case SkipChildren =>
                // .rightSibling looks at the parent node. If we're looking at thisTraversable,
                // then that means we should stop or we'll be traversing our parent's siblings.
                if node ne thisTraversable
                then impl(node.rightSibling)
                else ()
              case Continue => impl(node.firstChild)

          case leaf: (Node.Leaf & Node.Child) => fn(leaf)

      this match
        case thisChild: Node.Child => impl(thisChild)
        case thisTop: Node.Top =>
          thisTop.children.iterator
            .foreach(impl)
  end Traversable

  sealed trait All extends Cloneable:
    type This <: All
    def cloneEval(): Eval[This]

    final override def clone(): This =
      cloneEval().value

    final def inspect[T](pattern: Pattern[T]): Option[T] =
      pattern.check(this) match
        case Pattern.Result.Rejected => None
        case Pattern.Result.Accepted(value, _) =>
          Some(value)

    def asNode: Node =
      throw NodeError("not a node")

    def asTop: Top =
      throw NodeError("not a top")

    override def hashCode(): Int =
      this match
        case node: Node =>
          (Node, node.token, node.children).hashCode()
        case top: Top =>
          (Top, top.children).hashCode()
        case floating: Floating =>
          (Floating, floating.children).hashCode()
        case sentinel: RightSiblingSentinel =>
          (RightSiblingSentinel, sentinel.idxInParent).hashCode()
        case Embed(value) =>
          (Embed, value).hashCode()

    override def equals(that: Any): Boolean =
      if this eq that.asInstanceOf[AnyRef]
      then return true
      (this, that) match
        case (thisNode: Node, thatNode: Node) =>
          thisNode.token == thatNode.token
          && (if thisNode.token.showSource
              then thisNode.sourceRange == thatNode.sourceRange
              else true)
          && thisNode.children == thatNode.children
        case (thisTop: Top, thatTop: Top) =>
          thisTop.children == thatTop.children
        case (thisFloating: Floating, thatFloating: Floating) =>
          thisFloating.children == thatFloating.children
        case (
              thisSentinel: RightSiblingSentinel,
              thatSentinel: RightSiblingSentinel
            ) =>
          thisSentinel.idxInParent == thatSentinel.idxInParent
        case (thisEmbed: Embed[?], thatEmbed: Embed[?]) =>
          thisEmbed.value == thatEmbed.value
        case _ => false

    override def toString(): String =
      this match
        case node: Node =>
          val atSuffix =
            if node.token.showSource
            then s".at(${node.sourceRange.decodeString()})"
            else ""
          s"Node(${node.token})(${node.children.mkString(", ")})$atSuffix"
        case top: Top =>
          s"Top(${top.children.mkString(", ")})"
        case floating: Floating =>
          s"Floating(${floating.children.mkString(", ")})"
        case sentinel: RightSiblingSentinel =>
          s"RightSiblingSentinel(${sentinel.idxInParent})"
        case Embed(value) =>
          s"Embed($value)"

  sealed trait Root extends All:
    override type This <: Root
  sealed trait Leaf extends All:
    override type This <: Leaf
  sealed trait Sentinel extends All:
    override type This <: Sentinel

  final class Top(childrenInit: IterableOnce[Node.Child])
      extends Root,
        Parent(childrenInit),
        Traversable:
    def this(childrenInit: Node.Child*) = this(childrenInit)

    override def asTop: Top = this

    override def asNonFloatingParent: Node | Top = this

    override type This = Top
    override def cloneEval(): Eval[Top] =
      Chain
        .traverseViaChain(children.toIndexedSeq)(_.cloneEval())
        .map(_.toIterable)
        .map(Top(_))
  end Top

  object Top

  final class Floating(child: Node.Child)
      extends Root,
        Parent(Iterator.single(child)):

    override def asNonFloatingParent: Node | Top =
      throw NodeError("was floating parent")

    override type This = Floating
    override def cloneEval(): Eval[Floating] =
      children.head.cloneEval().map(_.parent.asInstanceOf[Floating])

  object Floating

  // TODO: is this a good or bad idea? Could save allocations, could waste memory...
  // object Floating:
  //   private val cache = java.util.WeakHashMap[ById[Node.Child], Floating]()
  //   def apply(child: Node.Child): Floating =
  //     cache.get(child) match
  //       case null =>
  //         val result = new Floating(child)
  //         cache.put(ById(child), result)
  //         result
  //       case result => result

  sealed trait Parent(childrenInit: IterableOnce[Node.Child]) extends All:
    thisParent =>

    def asNonFloatingParent: Node | Top

    override type This <: Parent

    val children: Node.Children = Node.Children(thisParent, childrenInit)

    final def apply(tok: Token): Node =
      val results = children.iterator
        .collect:
          case node: Node if node.token == tok =>
            node
        .toList

      require(results.size == 1)
      results.head

    final def firstChild: Node.Sibling =
      children.headOption match
        case None        => RightSiblingSentinel(this)
        case Some(child) => child

    @scala.annotation.tailrec
    private[Node] final def incScopeRelevance(): Unit =
      this match
        case root: Node.Root => // nothing to do here
        case thisNode: Node =>
          val oldScopeRelevance = thisNode._scopeRelevance
          thisNode._scopeRelevance += 1

          if oldScopeRelevance == 0
          then thisNode.parent.incScopeRelevance()

    @scala.annotation.tailrec
    private[Node] final def decScopeRelevance(): Unit =
      this match
        case root: Node.Root => // nothing to do here
        case thisNode: Node =>
          thisNode._scopeRelevance -= 1

          if thisNode._scopeRelevance == 0
          then thisNode.parent.decScopeRelevance()

    @scala.annotation.tailrec
    private[Node] final def findNodeByKey(key: Node): List[Node] =
      this match
        case root: Node.Root => Nil
        case thisNode: Node
            if thisNode.token.symbolTableFor.contains(key.token) =>
          import Node.TraversalAction.*
          val resultsList = mutable.ListBuffer.empty[Node]
          thisNode.traverseChildren:
            case irrelevantNode: Node if irrelevantNode._scopeRelevance == 0 =>
              SkipChildren
            case descendantNode: Node =>
              if !descendantNode.token.canBeLookedUp
              then TraversalAction.Continue
              else
                descendantNode.inspect(descendantNode.token.lookedUpBy) match
                  case None => TraversalAction.Continue
                  case Some(descendantKey) =>
                    if key == descendantKey
                    then resultsList.addOne(descendantNode)
                    TraversalAction.Continue
            case _: Node.Leaf => SkipChildren

          resultsList.result()
        case thisNode: Node =>
          thisNode.parent.findNodeByKey(key)

    final def traverseChildren(fn: Node.Child => TraversalAction): Unit =
      children.iterator.foreach(_.traverse(fn))

    final def children_=(childrenInit: IterableOnce[Node.Child]): Unit =
      children.clear()
      children.addAll(childrenInit)

    final def unparentedChildren: IterableOnce[Node.Child] =
      // .unparent() is done by Children .clear
      val result = children.toArray
      children.clear()
      result
  end Parent

  sealed trait Sibling extends All:
    thisSibling =>

    override type This <: Sibling

    def isChild: Boolean = false

    def parent: Parent
    def idxInParent: Int

    def rightSibling: Node.Sibling =
      if parent.children.isDefinedAt(idxInParent + 1)
      then parent.children(idxInParent + 1)
      else RightSiblingSentinel(parent)

    final def parents: Iterator[Parent] =
      Iterator.unfold(Some(parent): Option[Node.Parent]): curr =>
        curr.flatMap: curr =>
          curr match
            case parentNode: Node =>
              Some((parentNode, Some(parentNode.parent)))
            case root: (Top | Floating) =>
              Some((root, None))
  end Sibling

  final class RightSiblingSentinel private[Node] (val parent: Node.Parent)
      extends Sibling,
        Sentinel:

    override type This = RightSiblingSentinel
    override def cloneEval(): Eval[RightSiblingSentinel] =
      // No-one should reasonably even do this, but if they do, they will get back an identical object with the same parent.
      // Normally we want do have the clone not be parented, but this is unavoidable because sentinels only have parents.
      Eval.now(RightSiblingSentinel(parent))

    override def idxInParent: Int = parent.children.length
    override def rightSibling: Node.Sibling =
      throw NodeError(
        "tried to find the right sibling sentinel's right sibling"
      )
  end RightSiblingSentinel

  object RightSiblingSentinel

  sealed trait Child extends Sibling, Traversable, Leaf:
    thisChild =>

    override type This <: Child

    override def isChild: Boolean = true

    private var _parent: Parent = Node.Floating(thisChild)
    private var _idxInParent: Int = 0

    def ensureParent(parent: Parent, idxInParent: Int): this.type =
      if (_parent eq parent)
      then
        // reparenting within the same parent shouldn't really do anything,
        // so don't make a fuss if it happens. The seq ops on Children might do this.
        _idxInParent = idxInParent
        this
      else
        _parent match
          case _: Node.Floating =>
            _parent = parent
            _idxInParent = idxInParent
            this
          case _: (Node | Node.Top) =>
            throw NodeError("node already has a parent")

    def unparent(): this.type =
      _parent match
        case _: Node.Floating => // skip
        case _: (Node | Node.Top) =>
          _parent = Node.Floating(thisChild)
          _idxInParent = 0
      this

    override def parent: Parent = _parent

    override def idxInParent: Int = _idxInParent

    final def replaceThis(replacement: => Node.Child): Node.Child =
      val parentTmp = parent
      val idxInParentTmp = idxInParent
      val computedReplacement = replacement
      parentTmp.children(idxInParentTmp) = computedReplacement
      computedReplacement

    def iteratorErrors: Iterator[Node]
  end Child

  final case class Embed[T](value: T)(using val nodeMeta: NodeMeta[T])
      extends Child,
        Sibling,
        Leaf:
    override type This = Embed[T]
    override def cloneEval(): Eval[Embed[T]] =
      Eval.now(Embed(nodeMeta.doClone(value)))

    override def iteratorErrors: Iterator[Node] = Iterator.empty
  end Embed

  final class Children private[Node] (
      val parent: Node.Parent,
      childrenInit: IterableOnce[Node.Child]
  ) extends mutable.IndexedBuffer[Node.Child]:
    private val _children = mutable.ArrayBuffer.from(childrenInit)
    export _children.{length, apply}

    private def reIdxFromIdx(idx: Int): this.type =
      var curr = idx
      while curr < length
      do _children(curr).ensureParent(parent, curr)
      this

    // can't export due to redef rules
    override def knownSize: Int = _children.knownSize

    override def prepend(elem: Node.Child): this.type =
      _children.prepend(elem.ensureParent(parent, 0))
      reIdxFromIdx(1)

    override def insert(idx: Int, elem: Node.Child): Unit =
      _children.insert(idx, elem.ensureParent(parent, idx))
      reIdxFromIdx(idx + 1)

    @scala.annotation.tailrec
    override def insertAll(idx: Int, elems: IterableOnce[Node.Child]): Unit =
      elems match
        case elems: Iterable[Node.Child] =>
          // Keeping this separate allows ensureParent to fail without corrupting the structure.
          elems.iterator.zipWithIndex
            .foreach: (child, childIdx) =>
              child.ensureParent(parent, idx + childIdx)

          _children.insertAll(idx, elems)
          reIdxFromIdx(idx + elems.size)
        case elems => insertAll(idx, mutable.ArrayBuffer.from(elems))

    override def remove(idx: Int): Node.Child =
      val child = _children.remove(idx).unparent()
      reIdxFromIdx(idx)
      child

    override def remove(idx: Int, count: Int): Unit =
      (idx until idx + count).foreach: childIdx =>
        _children(childIdx).unparent()
      _children.remove(idx, count)
      reIdxFromIdx(idx)

    override def clear(): Unit =
      _children.foreach(_.unparent())
      _children.clear()

    override def addOne(elem: Child): this.type =
      _children.addOne(elem.ensureParent(parent, length))
      this

    override def update(idx: Int, child: Node.Child): Unit =
      val existingChild = _children(idx)
      if existingChild ne child
      then
        child.ensureParent(parent, idx)
        existingChild.unparent()
        _children(idx) = child

    override def iterator: Iterator[Node.Child] =
      (0 until length).iterator.map(this)

    final def findSibling(idx: Int): Node.Sibling =
      if isDefinedAt(idx)
      then this.apply(idx)
      else if idx >= length
      then RightSiblingSentinel(parent)
      else this.apply(idx) // negative idx
  end Children
end Node

trait NodeMeta[T](using val tag: Tag[T]):
  extension (self: T) def asNode: Node

  def doClone(self: T): T

object NodeMeta:
  given forToken: NodeMeta[Token] with
    extension (self: Token) def asNode: Node = self()

    def doClone(self: Token): Token = self

  final class byToString[T: Tag] extends NodeMeta[T]:
    def doClone(self: T): T = self
    extension (self: T)
      def asNode: Node =
        byToString.mkNode().at(self.toString())

  object byToString extends Token
