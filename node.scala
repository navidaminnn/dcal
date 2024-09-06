package distcompiler

import scala.collection.mutable

final class Node private[distcompiler] (
    val token: Token,
    private val childrenImpl: mutable.ArrayBuffer[Node.Child]
):
  thisNode =>

  private var parentOpt: Node | Null = null
  private var idxInParentOpt: Int = -1
  Node.onWorkQueue:
    var idxInParent = 0
    childrenImpl.mapInPlace: child =>
      val idx = idxInParent
      idxInParent += 1
      child.ensureParent(thisNode, idx)

  def ensureParent(parent: Node, idxInParent: Int): Node =
    if (parentOpt eq parent)
    then this
    else if parentOpt eq null
    then
      parentOpt = parent
      idxInParentOpt = idxInParent
      this
    else
      Node.withWorkQueue: _ =>
        Node(token, mutable.ArrayBuffer.from(childrenImpl))
          .like(thisNode)
          .ensureParent(parent, idxInParent)

  def reparent: this.type =
    // TODO: adjust symbol tables
    parentOpt = null
    idxInParentOpt = -1
    this

  def hasParent: Boolean =
    parentOpt ne null

  def parent: Node =
    parentOpt.nn

  def idxInParent: Int =
    assert(idxInParentOpt != -1)
    idxInParentOpt

  private var sourceRangeOpt: SourceRange | Null = null

  def at(sourceRange: SourceRange): Node =
    if sourceRangeOpt eq null
    then
      sourceRangeOpt = sourceRange
      this
    else
      new Node(token, mutable.ArrayBuffer.from(childrenImpl))
        .at(sourceRange)

  def like(other: Node): Node =
    if other.sourceRangeOpt eq null
    then this
    else at(other.sourceRangeOpt.nn)

  def sourceRange: SourceRange =
    if sourceRange ne null
    then sourceRange
    else
      var range = SourceRange.none
      val queue = mutable.Queue(this)

      while queue.nonEmpty
      do
        val node = queue.dequeue()

        if node.sourceRangeOpt ne null
        then range <+>= node.sourceRangeOpt.nn
        end if

        queue.enqueueAll:
          node.children.iterator
            .collect:
              case node: Node => node
      end while

      range

  object children
      extends IterableOnce[Node.Child],
        PartialFunction[Int, Node.Child]:
    export childrenImpl.{length, indices, iterator, isDefinedAt, forall, exists}

    override def apply(idx: Int): Node.Child =
      childrenImpl(idx)

    def update(idx: Int, child: Node.Child): Unit =
      childrenImpl(idx) = child.ensureParent(thisNode, idx)

    def patchInPlace(
        from: Int,
        patch: IterableOnce[Node.Child],
        replaced: Int
    ): this.type =
      childrenImpl.patchInPlace(
        from,
        patch.iterator.zipWithIndex
          .map: (child, idx) =>
            child.ensureParent(thisNode, from + idx),
        replaced
      )
      this
  end children
end Node

object Node:
  type Child = Node | Embed[?]

  final case class Embed[T](value: T)(using val nodeRepr: AsNode[T])

  extension (self: Child)
    def ensureParent(parent: Node, idxInParent: Int): Child =
      self match
        case self: Node     => self.ensureParent(parent, idxInParent)
        case self: Embed[?] => self

    def reparent: Child =
      self match
        case self: Node     => self.reparent
        case self: Embed[?] => self

    def isEmbed: Boolean =
      self match
        case _: Embed[?] => true
        case _: Node     => false

    def isNode: Boolean =
      self match
        case _: Node     => true
        case _: Embed[?] => false

    def asEmbed[T]: T =
      require(self.isEmbed)
      self.asInstanceOf[Embed[T]].value

    def asNode: Node =
      self match
        case self: Node     => self
        case self: Embed[t] => self.asNode
  end extension

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

trait AsNode[T]:
  extension (self: T) def asNode: Node
end AsNode

object AsNode:
  given token: AsNode[Token] with
    extension (self: Token) def asNode: Node = self()
end AsNode
