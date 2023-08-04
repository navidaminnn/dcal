package com.github.distcompiler.dcal

enum Token {
  case IntLiteral(value: BigInt)
  case StringLiteral(value: String)
  case Name(name: String)
  case Keyword(keyword: com.github.distcompiler.dcal.Keyword)
  case Punctuation(punctuation: com.github.distcompiler.dcal.Punctuation)
  case BinaryOperator(operator: com.github.distcompiler.dcal.BinaryOperator)
}
object Token {
  transparent trait Meta { self: Product =>
    def name: String = productPrefix
  }
}

enum Keyword extends Token.Meta {
  case call
  case await
  case `if`
  case `else`
  case `var`
  case `let`
  case `def`
  case `import`
  case `module`
}

enum Punctuation extends Token.Meta {
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
}

enum BinaryOperator extends Token.Meta {
  case `\\in`
  case `\\notin`
  case `=`
}
