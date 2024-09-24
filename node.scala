package distcompiler

import cats.Eval
import cats.data.Chain
import cats.syntax.all.given
import scala.collection.mutable
import izumi.reflect.Tag

final case class NodeError(msg: String) extends RuntimeException(msg)

final class Node(val token: Token)(childrenInit: IterableOnce[Node.Child] = Nil)
    extends Node.Child,
      Node.Sibling,
      Node.Parent(childrenInit),
      Node.Traversable,
      Equals:
  thisNode =>

  override type This = Node
  override def cloneEval(): Eval[Node] =
    Chain
      .traverseViaChain(children.toIndexedSeq)(_.cloneEval())
      .map(clonedChildren => Node(token)(clonedChildren.toIterable))

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
              case SkipChildren => impl(node.rightSibling)
              case Continue     => impl(node.firstChild)

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

    final def asNode: Node =
      require(this.isInstanceOf[Node])
      this.asInstanceOf[Node]

    final def asTop: Top =
      require(this.isInstanceOf[Top])
      this.asInstanceOf[Top]

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
    def this() = this(Nil)

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
    override type This = Floating
    override def cloneEval(): Eval[Floating] =
      children.head.cloneEval().map(_.parent.asInstanceOf[Floating])

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
        byToString.mkNode().at(Source(self.toString()).range)

  object byToString extends Token
