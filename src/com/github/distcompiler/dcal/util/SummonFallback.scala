package com.github.distcompiler.dcal.util

opaque type SummonFallback[First, Second] = Either[Second, First]

object SummonFallback {
  // due to specificity rules, this "subclass"" counts as more specific than above
  opaque type Primary[First, Second] <: SummonFallback[First, Second] = Either[Second, First]

  extension [First, Second](self: SummonFallback[First, Second]) {
    def value: Either[Second, First] = self
  }

  given summonFirst[First, Second](using first: First): Primary[First, Second] = Right(first)
  // since it returns the base class, the compiler will only use summonSecond if it can't find summonFirst
  // unlike other methods to achieve this, it will also print a mostly-comprehensible summoning diagnostic if something
  // bad happens, unlike say using a two-case summonFrom where the macro trace replaces the summoning diagnostic
  given summonSecond[First, Second](using second: Second): SummonFallback[First, Second] = Left(second)
}
