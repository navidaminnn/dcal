package com.github.distcompiler.dcal.parsing

final case class SourceLocation(path: String, offsetStart: Int, offsetEnd: Int) {
  // TODO: pretty-printing?

  def shiftRight: SourceLocation =
    copy(offsetStart = offsetStart + 1, offsetEnd = offsetEnd + 1)

  def combine(other: SourceLocation): SourceLocation = {
    require(path == other.path)
    SourceLocation(
      path = path,
      offsetStart = math.min(offsetStart, other.offsetStart),
      offsetEnd = math.max(offsetEnd, other.offsetEnd),
    )
  }
}

object SourceLocation {
  def fileStart(path: String, offsetStart: Int = 0): SourceLocation = SourceLocation(path = path, offsetStart, offsetStart)
}
