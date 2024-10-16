package distcompiler

import scala.util.Using

import java.nio.ByteBuffer
import java.nio.charset.StandardCharsets
import java.nio.channels.FileChannel
import java.nio.channels.FileChannel.MapMode

trait Source:
  def origin: Option[os.Path]
  def byteBuffer: ByteBuffer

  object lines:
    val nlOffsets: IArray[Int] =
      IArray.from:
        SourceRange
          .entire(Source.this)
          .iterator
          .zipWithIndex
          .collect:
            case ('\n', idx) => idx

    def lineColAtOffset(offset: Int): (Int, Int) =
      import scala.collection.Searching.*
      val lineIdx =
        nlOffsets.search(offset) match
          case Found(foundIndex)              => foundIndex
          case InsertionPoint(insertionPoint) => insertionPoint
      val colIdx =
        if lineIdx == 0
        then offset
        else offset - 1 - nlOffsets(lineIdx - 1)

      (lineIdx, colIdx)

    def lineStartOffset(lineIdx: Int): Int =
      if lineIdx == 0
      then 0
      else if lineIdx - 1 == nlOffsets.length
      then SourceRange.entire(Source.this).length
      else nlOffsets(lineIdx - 1) + 1
end Source

object Source:
  object empty extends Source:
    def origin: Option[os.Path] = None
    val byteBuffer: ByteBuffer =
      ByteBuffer.wrap(IArray.emptyByteIArray.unsafeArray)

  def fromString(string: String): Source =
    StringSource(string)

  def fromByteBuffer(byteBuffer: ByteBuffer): Source =
    ByteBufferSource(None, byteBuffer)

  def mapFromFile(path: os.Path): Source =
    Using.resource(FileChannel.open(path.toNIO)): channel =>
      val mappedBuf = channel.map(MapMode.READ_ONLY, 0, channel.size())
      ByteBufferSource(Some(path), mappedBuf)

  final class StringSource(val string: String) extends Source:
    def origin: Option[os.Path] = None
    lazy val byteBuffer: ByteBuffer =
      StandardCharsets.UTF_8.encode(string)

  final class ByteBufferSource(
      val origin: Option[os.Path],
      override val byteBuffer: ByteBuffer
  ) extends Source
