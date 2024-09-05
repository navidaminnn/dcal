package distcompiler

import scala.collection.mutable

final class Node private[distcompiler] (
    val token: Token,
    private val childrenImpl: mutable.ArrayBuffer[Node.Handle]
):
  thisNode =>

  private var parentOpt: Node | Null = null
  private var idxInParentOpt: Int = -1
  Node.onWorkQueue:
    childrenImpl.mapInPlace(_.ensureParent(thisNode))

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
    ensureMaterialized
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

        queue.enqueueAll(node.children.iteratorNodes)
      end while

      range

  def ensureMaterialized: this.type =
    if children.nonEmpty
    then
      Node.onWorkQueue:
        children.foreach(_.ensureMaterialized)
      this
    else this

  object children extends IterableOnce[Node], PartialFunction[Int, Node]:
    private def materializedChild(idx: Int): Node =
      val child = childrenImpl(idx)
      require(!child.isEmbed)
      val materializedChild =
        child.materialized.asNode
          .ensureParent(thisNode, idx)
      childrenImpl(idx) = materializedChild
      materializedChild

    def iteratorNodes: Iterator[Node] =
      (0 until length).iterator
        .filterNot: i =>
          childrenImpl(i).isEmbed
        .map(materializedChild)

    override def iterator: Iterator[Node] =
      (0 until length).iterator
        .map(apply)

    def length = childrenImpl.length

    def isEmbedAt(idx: Int): Boolean =
      childrenImpl(idx).isEmbed

    def embedAt[T](idx: Int): T =
      val child = childrenImpl(idx)
      require(child.isEmbed)
      child.asEmbed[T]

    override def isDefinedAt(idx: Int): Boolean =
      0 <= idx && idx < length

    override def apply(idx: Int): Node =
      val child = childrenImpl(idx)
      if child.isMaterialized
      then
        // embeds end up here as they are always evaluated; we lazily generate the nodes if asked
        child.asNode
      else materializedChild(idx)
    end apply

    def update(idx: Int, handle: Node.Handle): Unit =
      childrenImpl(idx) = handle

    def patchInPlace(
        from: Int,
        patch: IterableOnce[Node.Handle],
        replaced: Int
    ): this.type =
      childrenImpl.patchInPlace(from, patch, replaced)
      this
  end children
end Node

object Node:
  final case class Embed[T](value: T)(using val nodeRepr: AsNode[T])

  opaque type Handle = Node | (() => Node) | Embed[?]

  object Handle:
    given Conversion[Node, Handle] = identity
  end Handle

  extension (self: => Node) def lzy: Handle = () => self
  extension [T: AsNode](self: T) def embed: Handle = Embed(self)

  extension (self: Handle)
    def ensureParent(parent: Node): Handle =
      self match
        case self: Node         => self.ensureParent(parent)
        case self: (() => Node) => self
        case self: Embed[?]     => self

    def isMaterialized: Boolean =
      self match
        case _: (Node | Embed[?]) => true
        case _: (() => Node)      => false

    def isEmbed: Boolean =
      self match
        case _: Embed[?]              => true
        case _: ((() => Node) | Node) => false

    def asEmbed[T]: T =
      require(self.isEmbed)
      self.asInstanceOf[Embed[T]].value

    def materialized: Handle =
      self match
        case self: (() => Node)      => self()
        case self: (Node | Embed[?]) => self

    def asNode: Node =
      self match
        case self: Node         => self
        case self: Embed[t]     => self.nodeRepr.asNode(self.value)
        case self: (() => Node) => self()
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
