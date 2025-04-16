package distcompiler

import scala.collection.mutable
import dsl.*

trait WellformedDef:
  val topShape: Shape

  trait t protected (private[WellformedDef] val shape: Shape) extends Token

  protected def forceDef(tok: t): Unit =
    tok ::= tok.shape

  protected given builder: Wellformed.Builder =
    Wellformed.Builder(mutable.HashMap.empty, None)

  final lazy val wf: Wellformed =
    Node.Top ::= topShape

    val visited = mutable.HashSet[Token]()

    def implTok(tok: Token): Unit =
      if !visited(tok)
      then
        visited += tok
        tok match
          case tok: t =>
            tok ::= tok.shape
            implShape(tok.shape)
          case tok =>
            implShape(tok.existingShape)

    def implShape(shape: Shape): Unit =
      shape match
        case Shape.Atom     =>
        case Shape.AnyShape =>
        case Shape.Choice(choices) =>
          choices.foreach:
            case tok: Token => implTok(tok)
            case _          =>
        case Shape.Repeat(choice, minCount) =>
          implShape(choice)
        case Shape.Fields(fields) =>
          fields.foreach(implShape)

    implShape(topShape)

    builder.build()
  end wf
end WellformedDef
