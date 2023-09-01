package distcompiler.parsing

trait ErrorOps[Elem, Error] {
  def unexpectedEOF(sourceLocation: SourceLocation): Error
  def expectedEOF(sourceLocation: SourceLocation, actualElem: Elem): Error
}
