package distcompiler

import cats.syntax.all.given

object Builtin:
  object Tuple extends Token

  object Error extends Token:
    def apply(msg: String, ast: Node.Child): Node =
      Error(
        Error.Message().at(msg),
        Error.AST(ast)
      )

    object Message extends Token:
      override val showSource = true
    object AST extends Token
  end Error

  object SourceMarker extends Token.ShowSource
end Builtin
