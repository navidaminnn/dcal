package test.com.github.distcompiler.dcal.chungus

object SummonTuple {
  opaque type ST[T <: Tuple] = T

  object ST {
    given summonTupleEmpty: ST[EmptyTuple] = EmptyTuple
    given summonTupleCons[Hd, Tl <: Tuple](using hd: Hd, tl: ST[Tl]): ST[Hd *: Tl] = hd *: tl
  }

  extension [T <: Tuple](self: ST[T]) def value: T = self
}
