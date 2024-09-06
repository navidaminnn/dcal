package distcompiler

import scala.collection.Searching.{Found, InsertionPoint}

final class Source(val bytes: IArray[Byte], val origin: String)
    extends IterableOnce[Byte],
      PartialFunction[Int, Byte]:
  lazy val lines: IArray[SourceRange] =
    var rangeAcc = emptyRangeAt(0)

    val elems =
      bytes.iterator.zipWithIndex
        .tapEach: (_, idx) =>
          rangeAcc <+>= emptyRangeAt(idx)
        .map:
          case ('\n', idx) =>
            val result = Some(rangeAcc)
            rangeAcc = emptyRangeAt(idx + 1)
            result
          case _ => None
        .filter(_.nonEmpty)
        .map(_.get)
        .`++`: // this is eval'd once other elems are processed
          Iterator.single(rangeAcc)

    IArray.from(elems)
  end lines

  def lineNumAtIndex(idx: Int): Int =
    lines.search(emptyRangeAt(idx)) match
      case Found(foundIndex) =>
        foundIndex
      case InsertionPoint(insertionPoint) =>
        (insertionPoint - 1).max(0)

  def lineAtIndex(idx: Int): SourceRange =
    lines(lineNumAtIndex(idx))

  def lineColAtIndex(idx: Int): (Int, Int) =
    val lineNum = lineNumAtIndex(idx)
    val line = lines(lineNum)
    (lineNum, idx - line.start)

  def length: Int = bytes.length

  def isDefinedAt(idx: Int): Boolean = bytes.isDefinedAt(idx)

  def emptyRangeAt(idx: Int): SourceRange =
    new SourceRange(this, idx, idx)

  def apply(v1: Int): Byte = bytes.apply(v1)

  def iterator: Iterator[Byte] = bytes.iterator
end Source

object Source:
  val none: Source = new Source(IArray.empty, "<none>")

  def apply(source: String): Source =
    val bytes = source.getBytes()
    new Source(IArray.unsafeFromArray(bytes), "<synthetic>")

  def apply(path: os.Path): Source =
    new Source(IArray.unsafeFromArray(os.read.bytes(path)), path.toString)

  given convertToRange: Conversion[Source, SourceRange] with
    def apply(source: Source): SourceRange =
      new SourceRange(source, 0, source.length)
end Source

final class SourceRange(val source: Source, val start: Int, val length: Int)
    extends IterableOnce[Byte],
      PartialFunction[Int, Byte],
      Equals:
  require(start >= 0)
  if length == 0
  then require(end <= source.length)
  else require(end < source.length)

  def end: Int = start + length

  def lineColStart: (Int, Int) =
    source.lineColAtIndex(start)

  def lineColEnd: (Int, Int) =
    source.lineColAtIndex(start)

  override def canEqual(that: Any): Boolean =
    that.isInstanceOf[SourceRange]

  override def equals(that: Any): Boolean =
    that match {
      case that: SourceRange =>
        (source, start, end) == (that.source, that.start, that.end)
      case _ => false
    }

  override def hashCode(): Int =
    (source, start, end).hashCode()

  def toStringForDisplayShort: String =
    val (startLine, startCol) = source.lineColAtIndex(start)
    val (endLine, endCol) = source.lineColAtIndex(end)

    if start == end
    then s"${source.origin}:$startLine:$startCol"
    else s"${source.origin}:$startLine:$startCol-$endLine:$endCol"

  override def toString(): String = toStringForDisplayShort

  def toStringForDisplay: String =
    val startLine = source.lineAtIndex(start)
    val endLine = source.lineAtIndex(end)

    ???

  @scala.annotation.alpha("combine")
  def <+>(other: SourceRange): SourceRange =
    if source eq Source.none
    then other
    else if source eq other.source
    then
      if start <= other.start
      then
        new SourceRange(
          source,
          start,
          length.max(other.length + (other.start - start))
        )
      else
        new SourceRange(
          source,
          other.start,
          other.length.max(length + (start - other.start))
        )
    else this

  def containsSourceIndex(idx: Int): Boolean =
    start <= idx && idx < end

  def isDefinedAt(x: Int): Boolean = x >= 0 && x < length

  def apply(idx: Int): Byte = source(start + idx)

  def iterator: Iterator[Byte] =
    if start == end
    then Iterator.empty
    else
      (start until end).iterator
        .map(source)
end SourceRange

object SourceRange:
  val none: SourceRange = Source.none

  given ordering: Ordering[SourceRange] with
    def compare(x: SourceRange, y: SourceRange): Int =
      require(x.source eq y.source)

      val ord = Ordering[Int].compare(x.start, y.start)
      if ord == 0
      then Ordering[Int].compare(x.length, y.length)
      else ord
end SourceRange
