package distcompiler

import java.nio.ByteBuffer
import java.nio.channels.Channels
import java.nio.charset.{StandardCharsets, Charset}
import scala.collection.immutable.IndexedSeq

final class SourceRange private (
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
    slice(0, n)

  override def takeWhile(p: Byte => Boolean): SourceRange =
    take(iterator.takeWhile(p).size)

  override def drop(n: Int): SourceRange =
    require(n <= length)
    slice(n, length)

  override def dropRight(n: Int): SourceRange =
    require(n <= length)
    slice(0, length - n)

  override def init: SourceRange =
    dropRight(1)

  override def tail: SourceRange =
    drop(1)

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

object SourceRange:
  def entire(source: Source): SourceRange =
    SourceRange(source, 0, source.byteBuffer.limit())
