package distcompiler

import cats.syntax.all.given
import scala.annotation.targetName
import java.nio.charset.{StandardCharsets, Charset}
import java.nio.CharBuffer

trait Reader:
  protected def rules: Manip[SourceRange]

  final def apply(sourceRange: SourceRange): Node.Top =
    val top = Node.Top()

    val manip =
      Reader.srcRef.init(sourceRange):
        Reader.matchedRef.init(sourceRange.take(0)):
          rules

    manip.perform(top)
    top

object Reader:
  import dsl.*

  object srcRef extends Manip.Ref[SourceRange]
  object matchedRef extends Manip.Ref[SourceRange]

  // TODO: other ways to access matched ranges

  def consumeMatch[T](fn: SourceRange => Manip[T]): Manip[T] =
    matchedRef.get.lookahead.flatMap: m =>
      matchedRef.updated(m => m.drop(m.length))(fn(m))

  def dropMatch[T](manip: Manip[T]): Manip[T] =
    matchedRef.updated(m => m.drop(m.length))(manip)

  def extendThisNodeWithMatch[T](using DebugInfo)(manip: Manip[T]): Manip[T] =
    consumeMatch: m =>
      Manip.ThisNode.lookahead.flatMap:
        case node: Node =>
          effect(node.extendLocation(m))
            *> manip
        case top: Node.Top =>
          // if top is an extend target, ignore it.
          manip
        case _ => Manip.Backtrack(summon[DebugInfo])

  object bytes:
    private def advancingRefs[T](manip: Manip[T]): Manip[T] =
      srcRef.updated(_.tail):
        matchedRef.updated(_.extendRight):
          manip

    final class selecting[T] private (private val cell: selecting.Cell[T])
        extends AnyVal:
      @targetName("onOneOfChars")
      def onOneOf(chars: IterableOnce[Char])(manip: => Manip[T]): selecting[T] =
        onOneOf(chars.iterator.map(_.toByte))(manip)

      def on(char: Char)(manip: => Manip[T]): selecting[T] =
        on(char.toByte)(manip)

      def onOneOf(bytes: IterableOnce[Byte])(manip: => Manip[T]): selecting[T] =
        lazy val impl = manip
        bytes.iterator.foldLeft(this)(_.on(_)(impl))

      def on(byte: Byte)(manip: => Manip[T]): selecting[T] =
        new selecting(cell.add(Iterator.single(byte), defer(manip)))

      def onSeq(
          chars: CharSequence,
          encoding: Charset = StandardCharsets.UTF_8
      )(manip: => Manip[T]): selecting[T] =
        val bytes = SourceRange.entire(
          Source.fromByteBuffer(encoding.encode(CharBuffer.wrap(chars)))
        )
        new selecting(cell.add(bytes.iterator, defer(manip)))

      def fallback(using DebugInfo)(manip: => Manip[T]): Manip[T] =
        val fallbackImpl = defer(manip)
        srcRef.get.lookahead.flatMap: src =>
          val (matched, manip) = cell.lookup(src, fallbackImpl)
          srcRef.updated(_.drop(matched.length)):
            matchedRef.updated(_ <+> matched):
              manip

    object selecting:
      def apply[T]: selecting[T] =
        new selecting[T](Cell.Branch(Map.empty, None))

      enum Cell[T]:
        case Branch(map: Map[Byte, Cell[T]], fallbackOpt: Option[Manip[T]])
        case Leaf(manip: Manip[T])

        def lookup(
            src: SourceRange,
            fallback: Manip[T]
        ): (SourceRange, Manip[T]) =
          @scala.annotation.tailrec
          def impl(
              cell: Cell[T],
              src: SourceRange,
              matched: SourceRange,
              otherwise: (SourceRange, Manip[T])
          ): (SourceRange, Manip[T]) =
            cell match
              case Branch(map, fallbackOpt) =>
                def noMatch: (SourceRange, Manip[T]) =
                  fallbackOpt.fold(otherwise)((matched, _))

                if src.isEmpty
                then noMatch
                else
                  map.get(src.head) match
                    case None => noMatch
                    case Some(cell) =>
                      impl(cell, src.tail, matched.extendRight, noMatch)
              case Leaf(manip) => (matched, manip)

          impl(this, src, src.emptyAtOffset, (src.emptyAtOffset, fallback))

        def add(seq: Iterator[Byte], manip: Manip[T]): Cell[T] =
          if seq.hasNext
          then
            val seqHead = seq.next()
            this match
              case Branch(map, fallback) =>
                Branch(
                  map.updatedWith(seqHead)(
                    _.orElse(Some(Branch[T](Map.empty, None)))
                      .map(_.add(seq, manip))
                  ),
                  fallback
                )
              case Leaf(existingManip) =>
                Branch(
                  Map(seqHead -> Branch(Map.empty, None).add(seq, manip)),
                  Some(existingManip)
                )
          else
            this match
              case Branch(map, fallback) =>
                Branch(map, Some(fallback.fold(manip)(_ | manip)))
              case Leaf(manip) => Leaf(manip | manip)

    def selectManyLike[T](using DebugInfo)(bytes: Set[Byte])(
        manip: Manip[T]
    ): Manip[T] =
      lazy val impl: Manip[T] =
        srcRef.get.lookahead.flatMap: src =>
          if src.nonEmpty && bytes(src.head)
          then advancingRefs(impl)
          else Manip.Backtrack(summon[DebugInfo])
        | manip

      impl

    @targetName("selectManyLikeChars")
    def selectManyLike[T](using DebugInfo)(chars: Set[Char])(
        manip: Manip[T]
    ): Manip[T] =
      selectManyLike(chars.map(_.toByte))(manip)

    def selectSeq[T](using DebugInfo)(str: String)(manip: Manip[T]): Manip[T] =
      val bytes = str.getBytes()
      
      def impl(idx: Int): Manip[T] =
        if idx < bytes.length
        then
          srcRef.get.lookahead.flatMap: src =>
            if src.nonEmpty && src.head == bytes(idx)
            then advancingRefs(impl(idx + 1))
            else backtrack
        else manip

      impl(0)

    def selectCount[T](using DebugInfo)(count: Int)(manip: Manip[T]): Manip[T] =
      srcRef.get.lookahead.flatMap: src =>
        if count <= src.length
        then
          srcRef.updated(_.drop(count)):
            matchedRef.updated(_.extendRightBy(count)):
              manip
        else Manip.Backtrack(summon[DebugInfo])

    def selectOne[T](using DebugInfo)(manip: Manip[T]): Manip[T] =
      srcRef.get.lookahead.flatMap: src =>
        if src.nonEmpty
        then advancingRefs(manip)
        else backtrack

    def getSrc: Manip[SourceRange] =
      srcRef.get
