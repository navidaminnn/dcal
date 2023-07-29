package com.github.distcompiler.dcal.parsing

trait InputOps[Elem, Input] {
  def getPrevSourceLocation(input: Input): SourceLocation
  def read(input: Input): Option[(Elem, Input)]
}

object InputOps {
  final case class LazyListInput[Elem] private[InputOps] (prevSourceLocation: SourceLocation, list: LazyList[Elem])
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
          Some((char, LazyListInput(SourceLocation(path, offsetStart + 1, offsetEnd + 1), restChars)))
      }
  }

  given lazyLocatedListInputOps[Elem <: SourceLocated]: InputOps[Elem, LazyListInput[Elem]] with {
    override def getPrevSourceLocation(input: LazyListInput[Elem]): SourceLocation = input.prevSourceLocation

    override def read(input: LazyListInput[Elem]): Option[(Elem, LazyListInput[Elem])] =
      input match {
        case LazyListInput(_, LazyList()) => None
        case LazyListInput(_, elem #:: restElems) =>
          Some((elem, LazyListInput(elem.sourceLocation, restElems)))
      }
  }

  given lazyLocatedEitherListInputOps[Error, Elem <: SourceLocated]: InputOps[Either[Error, Elem], LazyListInput[Either[Error, Elem]]] with {
    override def getPrevSourceLocation(input: LazyListInput[Either[Error, Elem]]): SourceLocation = input.prevSourceLocation

    override def read(input: LazyListInput[Either[Error, Elem]]): Option[(Either[Error, Elem], LazyListInput[Either[Error, Elem]])] =
      input match {
        case LazyListInput(_, LazyList()) => None
        case LazyListInput(prevSourceLocation, Left(errors) #:: restElems) =>
          Some((Left(errors), LazyListInput(prevSourceLocation, restElems)))
        case LazyListInput(_, Right(elem) #:: restElems) =>
          Some((Right(elem), LazyListInput(elem.sourceLocation, restElems)))
      }
  }
}
