package com.github.distcompiler.dcal.parsing

trait InputOps[Elem, Input] {
  def getPrevSourceLocation(input: Input): SourceLocation
  def read(input: Input): Option[(Elem, Input)]
}

object InputOps {
  final case class LazyListInput[Elem] private (prevSourceLocation: SourceLocation, list: LazyList[Elem]) {
    def advanceWithPrevSourceLocation(prevSourceLocation: SourceLocation): LazyListInput[Elem] =
      copy(prevSourceLocation = prevSourceLocation, list = list.tail)
  }
  object LazyListInput {
    def apply[Elem](list: LazyList[Elem], offsetStart: Int = 0, path: String): LazyListInput[Elem] =
      LazyListInput(
        prevSourceLocation = SourceLocation(path = path, offsetStart = offsetStart, offsetEnd = offsetStart),
        list = list,
      )
  }

  given lazyCharListInputOps: InputOps[Char, LazyListInput[Char]] with {
    override def getPrevSourceLocation(input: LazyListInput[Char]): SourceLocation = input.prevSourceLocation

    override def read(input: LazyListInput[Char]): Option[(Char, LazyListInput[Char])] =
      input match {
        case LazyListInput(_, LazyList()) => None
        case LazyListInput(SourceLocation(path, offsetStart, offsetEnd), char #:: restChars) =>
          assert(offsetStart == offsetEnd)
          Some((char, input.advanceWithPrevSourceLocation(SourceLocation(path, offsetStart + 1, offsetEnd + 1))))
      }
  }
}
