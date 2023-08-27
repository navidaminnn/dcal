package distcompiler.parsing

trait InputOps[Elem, Input, Error] {
  def lastSourceLocation(input: Input): SourceLocation
  def nextSourceLocation(input: Input): SourceLocation
  def tellIdx(input: Input): Int
  def read(input: Input): Either[Error, Option[(Elem, Input)]]
}
