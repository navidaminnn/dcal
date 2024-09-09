package distcompiler

import scala.collection.mutable
import distcompiler.Node.RightSiblingSentinel
import distcompiler.Node.Embed
import distcompiler.Node.Top

final case class NodeError(msg: String) extends RuntimeException(msg)

final class Node(val token: Token)(childrenInit: IterableOnce[Node.Child])
    extends Node.Child,
      Node.Sibling,
      Node.Parent(childrenInit):
  thisNode =>

  private var _sourceRange: SourceRange | Null = null

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
    @scala.annotation.tailrec
    def impl(sibling: Node.Sibling, sourceRange: SourceRange): SourceRange =
      sibling match
        case node: Node =>
          val nextSourceRange =
            if node._sourceRange ne null
            then sourceRange <+> node._sourceRange.nn
            else sourceRange

          impl(node.firstChild, nextSourceRange)
        case sentinel: RightSiblingSentinel =>
          sentinel.parent match
            case parentNode: Node =>
              impl(parentNode.rightSibling, sourceRange)
            case _: Top =>
              sourceRange
        case Embed(value) =>
          impl(sibling.rightSibling, sourceRange)

    impl(thisNode, sourceRange = SourceRange.none)
  end sourceRange

  override def iteratorErrors: Iterator[Node] =
    if token == Builtin.error
    then Iterator.single(thisNode)
    else children.iterator.foldLeft(Iterator.empty[Node])(_ ++ _.iteratorErrors)
end Node

object Node:
  final class Top(childrenInit: IterableOnce[Node.Child])
      extends Parent(childrenInit):
    def this() = this(Nil)
  end Top

  sealed trait Parent(childrenInit: IterableOnce[Node.Child]):
    thisParent =>

    private val _children = mutable.ArrayBuffer.from[Node.Child](childrenInit)
    locally:
      _children.iterator.zipWithIndex
        .foreach: (child, idx) =>
          child.ensureParent(this, idx)

    def firstChild: Node.Sibling =
      children.headOption match
        case None        => RightSiblingSentinel(this)
        case Some(child) => child

    def children_=(childrenInit: IterableOnce[Node.Child]): Unit =
      children.iterator.foreach(_.unparent)
      _children.clear()
      _children.addAll(childrenInit)
      _children.iterator.zipWithIndex
        .foreach: (child, idx) =>
          child.ensureParent(thisParent, idx)

    def unparentedChildren: IterableOnce[Node.Child] =
      children.iterator.foreach(_.unparent)
      _children

    object children
        extends IterableOnce[Node.Child],
          PartialFunction[Int, Node.Child]:
      export _children.{
        iterator,
        length,
        isDefinedAt,
        forall,
        exists,
        head,
        headOption,
        last,
        lastOption,
        isEmpty,
        nonEmpty
      }
      override def apply(idx: Int): Node.Child =
        _children(idx)

      def findSibling(idx: Int): Node.Sibling =
        if _children.isDefinedAt(idx)
        then apply(idx)
        else if idx >= _children.length
        then RightSiblingSentinel(thisParent)
        else apply(idx) // negative idx

      def addOne(child: Node.Child): Unit =
        _children.addOne(child.ensureParent(thisParent, _children.length))

      def update(idx: Int, child: Node.Child): Unit =
        // make sure the old child doesn't think it can still refer up this tree
        _children(idx).unparent
        _children(idx) = child.ensureParent(thisParent, idx)

      def patchInPlace(
          from: Int,
          patch: IterableOnce[Node.Child],
          replaced: Int
      ): this.type =
        // unparent the children that will be removed
        (from until from + replaced).foreach: idx =>
          _children(idx).unparent

        var patchLength = 0
        _children.patchInPlace(
          from,
          patch.iterator
            .tapEach: child =>
              child.ensureParent(thisParent, from + patchLength)
              patchLength += 1
          ,
          replaced
        )

        // If we changed the indices of any nodes after the patched section,
        // make sure they know it (unparent first because we are reassigning, effectively)
        if patchLength != replaced
        then
          (from + patchLength until _children.length).foreach: idx =>
            _children(idx).unparent
              .ensureParent(thisParent, idx)

        this
    end children
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

    def parents: Iterator[Parent] =
      Iterator.unfold(Some(parent): Option[Node.Parent]): curr =>
        curr.flatMap: curr =>
          curr match
            case parentNode: Node =>
              Some((parentNode, Some(parentNode.parent)))
            case top: Top =>
              Some((top, None))

  end Sibling

  final class RightSiblingSentinel private[Node] (val parent: Node.Parent)
      extends Sibling:
    override def idxInParent: Int = parent.children.length
    override def rightSibling: Node.Sibling =
      throw NodeError(
        "tried to find the right sibling sentinel's right sibling"
      )
  end RightSiblingSentinel

  sealed trait Child extends Sibling:
    thisChild =>

    override def isChild: Boolean = true

    private var _parent: Parent | Null = null
    private var _idxInParent: Int = -1

    def ensureParent(parent: Parent, idxInParent: Int): this.type =
      if (_parent eq parent) && _idxInParent == idxInParent
      then this
      else if _parent eq null
      then
        assert(_idxInParent == -1)
        _parent = parent
        _idxInParent = idxInParent
        this
      else throw NodeError("node already has a parent")

    def unparent: this.type =
      // TODO: adjust symbol tables
      _parent = null
      _idxInParent = -1
      this

    override def parent: Parent =
      if parent eq null
      then throw NodeError("tried to find a floating node's parent")
      _parent.nn

    def replaceThis(replacement: => Node.Child): Node.Child =
      val parentTmp = parent
      val idxInParentTmp = idxInParent
      val computedReplacement = replacement
      parentTmp.children(idxInParentTmp) = computedReplacement
      computedReplacement

    override def idxInParent: Int =
      assert(_idxInParent != -1)
      _idxInParent

    def iteratorErrors: Iterator[Node]
  end Child

  final case class Embed[T](value: T)(using val nodeRepr: AsNode[T])
      extends Child,
        Sibling:
    override def iteratorErrors: Iterator[Node] = Iterator.empty
  end Embed

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
