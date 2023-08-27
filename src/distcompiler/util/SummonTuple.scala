package distcompiler.util

opaque type SummonTuple[T <: Tuple] = T

object SummonTuple {
  given summonTupleEmpty: SummonTuple[EmptyTuple] = EmptyTuple
  given summonTupleCons[Hd, Tl <: Tuple](using hd: Hd, tl: SummonTuple[Tl]): SummonTuple[Hd *: Tl] = hd *: tl

  extension [T <: Tuple](self: SummonTuple[T]) def value: T = self
}
