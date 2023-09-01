package distcompiler.dcal

import distcompiler.transform.Transformable

enum Token derives CanEqual, Transformable {
  case IntLiteral(value: BigInt)
  case StringLiteral(value: String)
  case Name(name: String)
  case Keyword(keyword: distcompiler.dcal.Keyword)
  case Punctuation(punctuation: distcompiler.dcal.Punctuation)
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
  case `check`
  case `alias`
  case `override`
  case `abstract`
  case `interface`
  case `extends`
  case `assume`
  case `assert`
  case `either`
  case `or`
  case `defer`
  case `return`
  case `fork`
  case `and`
  case `spawn`

  case `CHOOSE`
  case `EXCEPT`
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
  case `;`
  case `<<`
  case `>>`
  case `:`
  case `\\E`
  case `\\A`
  case `!`
  case `|->`
  case `@!`
  case `@`

  case `\\in` extends Punctuation, BinaryOperator(Precedence(5, 5))
  case `\\notin` extends Punctuation, BinaryOperator(Precedence(5, 5))
  case `=` extends Punctuation, BinaryOperator(Precedence(5, 5))
  case `.` extends Punctuation, BinaryOperator(Precedence(17, 17), leftAssociative = true)
}

sealed trait BinaryOperator(val precedence: Precedence, val leftAssociative: Boolean = false) derives CanEqual, Transformable { self: Punctuation =>
  def asPunctuation: Punctuation = self
}

