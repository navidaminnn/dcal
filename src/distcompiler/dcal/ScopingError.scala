package distcompiler.dcal

import distcompiler.parsing.Ps

enum ScopingError {
  case Redefinition(first: Ps[IdAST.NameBinder], second: Ps[IdAST.NameBinder])
  case UndefinedReference(ref: Ps[IdAST.PathSegment])
}
