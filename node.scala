package distcompiler

import scala.collection.mutable

final case class NodeError(msg: String) extends RuntimeException(msg)

final class Node(val token: Token)(childrenInit: IterableOnce[Node.Child])
    extends Node.Child,
      Node.Sibling,
      Node.Parent(childrenInit),
      Node.Traversable,
      Equals:
  thisNode =>

  private var _sourceRange: SourceRange = SourceRange.none

  def at(sourceRange: SourceRange): this.type =
    if _sourceRange eq SourceRange.none
    then
      _sourceRange = sourceRange
      this
    else throw NodeError("node source range already set")

  def like(other: Node): this.type =
    if other._sourceRange eq null
    then this
    else at(other._sourceRange.nn)

  def sourceRange: SourceRange =
    @scala.annotation.tailrec
    def impl(sibling: Node.Sibling, sourceRange: SourceRange): SourceRange =
      sibling match
        case node: Node =>
          val nextSourceRange =
            if node._sourceRange ne null
            then sourceRange <+> node._sourceRange.nn
            else sourceRange

          impl(node.firstChild, sourceRange <+> node._sourceRange)
        case sentinel: Node.RightSiblingSentinel =>
          sentinel.parent match
            case parentNode: Node =>
              impl(parentNode.rightSibling, sourceRange)
            case _: Node.Root =>
              sourceRange
        case _: Node.Leaf =>
          impl(sibling.rightSibling, sourceRange)

    impl(thisNode, sourceRange = SourceRange.none)
  end sourceRange

  override def canEqual(that: Any): Boolean =
    that.isInstanceOf[Node]

  override def equals(that: Any): Boolean =
    that match
      case thatNode: Node =>
        token == thatNode.token
        && (!token.showSource || sourceRange == thatNode.sourceRange)
        && iteratorDescendants
          .map(Some(_))
          .zipAll(thatNode.iteratorDescendants.map(Some(_)), None, None)
          .forall:
            case (None, None)    => true
            case (None, Some(_)) => false
            case (Some(_), None) => false
            case (Some(lNode: Node), Some(rNode: Node)) =>
              lNode.token == rNode.token
              && (!lNode.token.showSource || lNode.sourceRange == rNode.sourceRange)
            case (Some(lLeaf: Node.Leaf), Some(rLeaf: Node.Leaf)) =>
              lLeaf == rLeaf
            case (Some(_), Some(_)) => false
      case _ => false

  override def hashCode(): Int =
    (token, children).hashCode()

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
        case leaf: Node.Leaf => Some((Some(leaf), leaf.rightSibling))
        case node: Node      => Some((Some(node), node.firstChild))
      .flatten

  override def iteratorErrors: Iterator[Node] =
    if token == Builtin.error
    then Iterator.single(thisNode)
    else children.iterator.foldLeft(Iterator.empty[Node])(_ ++ _.iteratorErrors)

  def lookup: List[Node] =
    assert(token.canBeLookedUp)
    parent.findNodeByKey(thisNode)

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
              case SkipChildren => impl(node.rightSibling)
              case Continue     => impl(node.firstChild)

          case leaf: Node.Leaf => fn(leaf)

      this match
        case thisChild: Node.Child => impl(thisChild)
        case thisTop: Node.Top =>
          thisTop.children.iterator
            .foreach(impl)
  end Traversable

  object TraversalAction:
    given unitMeansContinue: Conversion[Unit, TraversalAction] = _ =>
      TraversalAction.Continue

  sealed trait Root extends Node.Parent

  sealed trait Leaf extends Node.Child

  final class Top(childrenInit: IterableOnce[Node.Child])
      extends Root,
        Parent(childrenInit),
        Traversable:
    def this() = this(Nil)
  end Top

  final class Floating(child: Node.Child)
      extends Root,
        Parent(Iterator.single(child))

  sealed trait Parent(childrenInit: IterableOnce[Node.Child]):
    thisParent =>

    val children: Node.Children = Node.Children(thisParent, childrenInit)

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
            case _: Node.Leaf => SkipChildren
            case irrelevantNode: Node if irrelevantNode._scopeRelevance == 0 =>
              SkipChildren
            case descendantNode: Node =>
              descendantNode.token.lookedUpBy match
                case None => // no lookup
                case Some(lookedUpBy) =>
                  lookedUpBy(descendantNode) match
                    case None => // can't find key. This should at least fail a WF check.
                    case Some(descendantKey) =>
                      if key == descendantKey
                      then resultsList.addOne(descendantNode)

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

  sealed trait Sibling:
    thisSibling =>

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
      extends Sibling:
    override def idxInParent: Int = parent.children.length
    override def rightSibling: Node.Sibling =
      throw NodeError(
        "tried to find the right sibling sentinel's right sibling"
      )
  end RightSiblingSentinel

  sealed trait Child extends Sibling, Traversable:
    thisChild =>

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

  final case class Embed[T](value: T)(using val nodeRepr: AsNode[T])
      extends Child,
        Sibling,
        Leaf:
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

  private val recursionBreakingWorkQueue =
    ThreadLocal.withInitial[mutable.Queue[() => Unit] | Null](() => null)

  private def withWorkQueue[T](fn: mutable.Queue[() => Unit] => T): T =
    recursionBreakingWorkQueue.get() match
      case null =>
        val ownWorkQueue = mutable.Queue.empty[() => Unit]
        recursionBreakingWorkQueue.set(ownWorkQueue)

        val result = fn(ownWorkQueue)

        while (ownWorkQueue.nonEmpty)
          ownWorkQueue.dequeue().apply()

        Node.recursionBreakingWorkQueue.set(null)

        result
      case workQueue =>
        fn(workQueue)
    end match

  private def onWorkQueue(task: => Unit): Unit =
    withWorkQueue: workQueue =>
      workQueue.enqueue(() => task)
end Node

trait AsNode[T]:
  extension (self: T) def asNode: Node
end AsNode

object AsNode:
  given token: AsNode[Token] with
    extension (self: Token) def asNode: Node = self()
end AsNode
