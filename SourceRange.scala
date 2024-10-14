package distcompiler

import java.nio.ByteBuffer
import java.nio.channels.Channels
import java.nio.charset.{StandardCharsets, Charset}
import scala.collection.immutable.IndexedSeq
import scala.collection.mutable

final class SourceRange(
    val source: Source,
    val offset: Int,
    val length: Int
) extends IndexedSeq[Byte],
      geny.Writable:
  require(
    0 <= offset && 0 <= length && offset + length <= source.byteBuffer.limit(),
    s"invalid offset = $offset, length = $length into source with limit ${source.byteBuffer.limit()}"
  )

  override def toString(): String =
    s"SourceRange(${{ decodeString() }})"

  def apply(i: Int): Byte =
    source.byteBuffer
      .get(offset + i)

  def byteBuffer(): ByteBuffer =
    source.byteBuffer.slice(offset, length)

  def decodeString(charset: Charset = StandardCharsets.UTF_8): String =
    charset.decode(byteBuffer()).toString()

  def writeBytesTo(out: java.io.OutputStream): Unit =
    val channel = Channels.newChannel(out)
    val bufSlice = this.byteBuffer()
    // It seems .write will make a significant effort to write all bytes,
    // but just to be safe let's retry if it writes less.
    var count = 0
    while count < length
    do count += channel.write(bufSlice)

  def emptyAtOffset: SourceRange =
    dropRight(length)

  def extendLeftBy(count: Int): SourceRange =
    SourceRange(source, offset - count, length)

  def extendLeft: SourceRange =
    extendLeftBy(1)

  def extendRightBy(count: Int): SourceRange =
    SourceRange(source, offset, length + count)

  def extendRight: SourceRange =
    extendRightBy(1)

  override def slice(from: Int, until: Int): SourceRange =
    require(0 <= from && from <= until && until <= length)
    SourceRange(source, offset + from, until - from)

  override def take(n: Int): SourceRange =
    if n >= length
    then this
    else slice(0, n)

  override def takeWhile(p: Byte => Boolean): SourceRange =
    take(iterator.takeWhile(p).size)

  override def drop(n: Int): SourceRange =
    if n <= length
    then slice(n, length)
    else emptyAtOffset

  override def dropRight(n: Int): SourceRange =
    if n <= length
    then slice(0, length - n)
    else emptyAtOffset

  override def init: SourceRange =
    dropRight(1)

  override def tail: SourceRange =
    drop(1)

  @scala.annotation.targetName("combine")
  def <+>(other: SourceRange): SourceRange =
    if source eq Source.empty
    then other
    else if other.source eq Source.empty
    then this
    else if source eq other.source
    then
      // convert to [start,end) spans so we can do min/max merge.
      // Doesn't work on lengths because those are relative to offset,
      // but start / end are independent.
      val (startL, endL) = (offset, offset + length)
      val (startR, endR) = (other.offset, other.offset + other.length)
      val (startC, endC) = (startL.min(startR), endL.max(endR))
      SourceRange(source, startC, endC - startC)
    else this

  def presentationStringShort: String =
    val builder = StringBuilder()
    builder ++= source.origin.map(_.toString).getOrElse("<internal>")
    builder += ':'

    val (lineIdx, colIdx) = source.lines.lineColAtOffset(offset)
    builder.append(lineIdx + 1)
    builder += ':'
    builder.append(colIdx + 1)

    if length > 0
    then
      builder += '-'
      val (lineIdx2, colIdx2) = source.lines.lineColAtOffset(offset + length)
      if lineIdx == lineIdx2
      then builder.append(colIdx2 + 1)
      else
        builder.append(lineIdx2 + 1)
        builder += ':'
        builder.append(colIdx2 + 1)

    builder.result()

object SourceRange:
  final class Builder extends mutable.Builder[Byte, SourceRange]:
    private val arrayBuilder = Array.newBuilder[Byte]
    def addOne(elem: Byte): this.type =
      arrayBuilder.addOne(elem)
      this
    def clear(): Unit = arrayBuilder.clear()
    def result(): SourceRange =
      SourceRange.entire(
        Source.fromByteBuffer(ByteBuffer.wrap(arrayBuilder.result()))
      )

  def newBuilder: Builder = Builder()

  def entire(source: Source): SourceRange =
    SourceRange(source, 0, source.byteBuffer.limit())

  extension (ctx: StringContext)
    def src: srcImpl =
      srcImpl(ctx)

  final class srcImpl(val ctx: StringContext) extends AnyVal:
    def unapplySeq(sourceRange: SourceRange): Option[Seq[SourceRange]] =
      val parts = ctx.parts
      assert(parts.nonEmpty)

      extension (part: String)
        def bytes: SourceRange =
          SourceRange.entire:
            Source.fromByteBuffer:
              StandardCharsets.UTF_8.encode:
                StringContext.processEscapes(part)

      val firstPart = parts.head.bytes

      if !sourceRange.startsWith(firstPart)
      then return None

      var currRange = sourceRange.drop(firstPart.length)
      val matchedParts = mutable.ListBuffer.empty[SourceRange]
      val didMatchFail =
        parts.iterator
          .drop(1)
          .map(_.bytes)
          .map: part =>
            if part.isEmpty
            then
              val idx = currRange.length
              matchedParts += currRange
              currRange = currRange.drop(currRange.length)
              idx
            else
              val idx = currRange.indexOfSlice(part)
              if idx != -1
              then
                matchedParts += currRange.take(idx)
                currRange = currRange.drop(idx)

              idx
          .contains(-1)
          || currRange.nonEmpty

      if didMatchFail
      then None
      else Some(matchedParts.result())
