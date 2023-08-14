package com.github.distcompiler.dcal.transform

object SummonFallback {
  sealed class SF[+First, +Second](val value: Either[Second, First])
  
  // due to specificity rules, this subclass counts as "more specific" than SF itself
  final class SFPrio[+First, +Second](value: Either[Second, First]) extends SF[First, Second](value)

  object SF {
    given summonFirst[First, Second](using first: First): SFPrio[First, Second] = SFPrio(Right(first))
    // since it returns the base class, the compiler will only use summonSecond if it can't find summonFirst
    // unlike other methods to achieve this, it will also print a mostly-comprehensible summoning diagnostic if something
    // bad happens, unlike say using a two-case summonFrom where the macro trace replaces the summoning diagnostic
    given summonSecond[First, Second](using second: Second): SF[First, Second] = SF(Left(second))
  }
}
