package com.github.distcompiler.dcal

import parsing.PsK

enum ScopingError {
  case Redefinition(first: PsK[Scoping.Def], second: PsK[Scoping.Def])
  case UndefinedReference(ref: PsK[Scoping.Referent])
  case ArityMismatch(ref: PsK[Scoping.Referent], defn: PsK[Scoping.Def])
  case KindMismatch(ref: PsK[Scoping.Referent], defn: PsK[Scoping.Def])
}
