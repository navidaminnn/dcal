package com.github.distcompiler.dcal.parsing

final case class Ps[+T](value: T)(using val sourceLocation: SourceLocation)

object Ps {
  given unwrap[T]: Conversion[Ps[T], T] with {
    def apply(x: Ps[T]): T = x.value
  }
}
