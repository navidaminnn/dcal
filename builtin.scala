package distcompiler

import cats.syntax.all.given

object Builtin:
  object tuple extends Token

  object Error extends Token:
    def apply(msg: String, ast: Node.Child): Node =
      Error(
        Error.Message().at(Source(msg).range),
        Error.AST(ast)
      )

    object Message extends Token:
      override val showSource = true
    object AST extends Token
  end Error

  object SourceMarker extends Token

  object Lift extends Token:
    def apply(dest: Token, node: Node, nodeInPlace: Node): Node =
      Lift(
        Lift.DestinationToken(dest()),
        Lift.Payload(node),
        Lift.Replacement(nodeInPlace)
      )

    object DestinationToken extends Token
    object Payload extends Token
    object Replacement extends Token

    import Manip.*
    import Pattern.*
    lazy val rules: Rules =
      on(
        (tok(Lift) *> firstChild(tok(Lift.DestinationToken)))
          .flatMap(dest => ancestor(tok(dest.token)))
        *: children:
          (tok(Lift.DestinationToken) <* children(atEnd))
            *>: tok(Lift.Payload)
            **: tok(Lift.Replacement)
            <*: atEnd
      ).rewrite: (destination, payload, replacement) =>
        destination.children.addOne(payload.unparent())
        replacement.unparent()
      | on(tok(Lift))
        .rewrite: badLift =>
          Error("malformed lift node", badLift.unparent())
  end Lift
end Builtin
