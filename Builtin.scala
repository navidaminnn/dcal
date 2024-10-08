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

    import dsl.*
    lazy val rules: Rules =
      on(
        (tok(Lift) *> firstChild(tok(Lift.DestinationToken)))
          .flatMap(dest => ancestor(tok(dest.token)))
        *: children:
          Fields()
            .skip(tok(Lift.DestinationToken) <* children(atEnd))
            .field(tok(Lift.Payload))
            .field(tok(Lift.Replacement))
            .atEnd
      ).rewrite: (destination, payload, replacement) =>
        destination.children.addOne(payload.unparent())
        Splice(replacement.unparent())
      | on(tok(Lift))
        .rewrite: badLift =>
          Splice(Error("malformed lift node", badLift.unparent()))
  end Lift
end Builtin
