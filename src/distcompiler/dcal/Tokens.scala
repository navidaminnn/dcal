package distcompiler.dcal

import distcompiler.transform.Transformable

enum Token derives CanEqual, Transformable {
  case IntLiteral(value: BigInt)
  case StringLiteral(value: String)
  case Name(name: String)
  case Keyword(keyword: distcompiler.dcal.Keyword)
  case Punctuation(punctuation: distcompiler.dcal.Punctuation)
  case BinaryOperator(operator: distcompiler.dcal.BinaryOperator)
}
object Token {
  transparent trait Meta { self: Product =>
    def name: String = productPrefix
  }
}

enum Keyword extends Token.Meta derives CanEqual, Transformable {
  case `call`
  case `await`
  case `if`
  case `else`
  case `var`
  case `let`
  case `def`
  case `import`
  case `module`
  case `instance`
  case `impl`
  case `pure`
  case `async`
}

object Keyword {
  val stringSet: Set[String] =
    Keyword.values.iterator.map(_.name).toSet
}

enum Punctuation extends Token.Meta derives CanEqual, Transformable {
  case `,`
  case `(`
  case `)`
  case `[`
  case `]`
  case `{`
  case `}`
  case `:=`
  case `||`
  case `.`
  case `;`
}

enum BinaryOperator extends Token.Meta derives CanEqual, Transformable {
  case `\\in`
  case `\\notin`
  case `=`
}
