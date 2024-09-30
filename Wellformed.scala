package distcompiler

import cats.syntax.all.given
import scala.collection.mutable
import Builtin.Error

final class Wellformed private (
    assigns: Map[Token, Wellformed.Shape],
    topShape: Wellformed.Shape
):
  import Wellformed.Shape
  import dsl.*

  private lazy val tokensByName: Map[SourceRange, Token] =
    assigns.map: (token, _) =>
      SourceRange.entire(Source.fromString(token.name)) -> token

  private lazy val namesByToken: Map[Token, SourceRange] =
    assigns.map: (token, _) =>
      token -> SourceRange.entire(Source.fromString(token.name))

  lazy val markErrors: Manip[Unit] =
    def shapePattern(shape: Wellformed.Shape): Pattern[Node.Sibling] =
      shape match
        case Shape.Atom =>
          atEnd
        case Shape.Choice(choices) =>
          oneOfToks(choices)
        case Shape.Repeat(choice) =>
          shapePattern(choice)
        case Shape.Fields(fields) =>
          fields.iterator.zipWithIndex
            .map: (field, idx) =>
              shapePattern(field).restrict:
                case sibling if sibling.idxInParent == idx => sibling
            .reduceLeftOption(_ | _)
            .getOrElse(Pattern.Reject)

    pass(once = true)
      .rules:
        on(
          assigns.foldLeft(shapePattern(topShape) <* parent(theTop)):
            case (acc, (token, shape)) =>
              acc
                | (shapePattern(shape) <* parent(tok(token)))
        ).rewrite: node =>
          Splice(
            Error(
              "unexpected shape",
              node match
                case _: Node.Sentinel  => Builtin.SourceMarker()
                case child: Node.Child => child.unparent()
            )
          )
  end markErrors

  def makeDerived(fn: Wellformed.Builder ?=> Unit): Wellformed =
    val builder =
      Wellformed.Builder(mutable.HashMap.from(assigns), Some(topShape))
    fn(using builder)
    builder.build()

  def checkTree(top: Node.Top): Unit =
    markErrors.perform(top)

  private lazy val rewriteFromSExpressions: Manip[Unit] =
    pass(strategy = pass.bottomUp, once = true)
      .rules:
        on(
          tok(sexpr.tokens.List).map(_.sourceRange)
          *: children:
            tok(sexpr.tokens.Atom)
              .map(_.sourceRange)
              .restrict(tokensByName)
              *: repeated(anyChild)
        ).rewrite: (src, token, listOfChildren) =>
          Splice(token(listOfChildren.iterator.map(_.unparent())).at(src))

  def fromSExpression(top: Node.Top): Unit =
    locally:
      sexpr.wellFormed.markErrors
        *> rewriteFromSExpressions
        *> markErrors
    .perform(top)

  private lazy val rewriteToSExpressions: Manip[Unit] =
    pass(once = true)
      .rules:
        on(
          anyNode
            *: anyNode
              .map(_.token)
              .restrict(namesByToken)
        ).rewrite: (node, name) =>
          val list = sexpr.tokens.List()
          list.children.addOne(sexpr.tokens.Atom(name))
          list.children.addAll(node.unparentedChildren)
          Splice(list)

  def toSExpression(top: Node.Top): Unit =
    locally:
      markErrors
        *> rewriteToSExpressions
        *> sexpr.wellFormed.markErrors
    .perform(top)
end Wellformed

object Wellformed:
  def apply(fn: Builder ?=> Unit): Wellformed =
    val builder = Builder(mutable.HashMap.empty, None)
    fn(using builder)
    builder.build()

  final class Builder private[Wellformed] (
      assigns: mutable.HashMap[Token, Wellformed.Shape],
      private var topShapeOpt: Option[Wellformed.Shape]
  ):
    extension (top: Node.Top.type)
      def ::=(shape: Shape): Unit =
        require(topShapeOpt.isEmpty)
        topShapeOpt = Some(shape)
      def ::=!(shape: Shape): Unit =
        require(topShapeOpt.nonEmpty)
        topShapeOpt = Some(shape)

    extension (token: Token)
      def ::=(shape: Shape): Unit =
        require(!assigns.contains(token))
        assigns(token) = shape

      def ::=!(shape: Shape): Unit =
        require(assigns.contains(token))
        assigns(token) = shape

    private[Wellformed] def build(): Wellformed =
      require(topShapeOpt.nonEmpty)
      new Wellformed(assigns.toMap, topShape = topShapeOpt.get)
  end Builder

  enum Shape:
    case Atom
    case Choice(choices: Set[Token])
    case Repeat(choice: Shape.Choice)
    case Fields(fields: List[Shape.Choice])

  object Shape:
    extension (choice: Shape.Choice)
      def |(other: Shape.Choice): Shape.Choice =
        Shape.Choice(choice.choices ++ other.choices)

    def repeated(choice: Shape.Choice): Shape.Repeat =
      Shape.Repeat(choice)

    def fields(fields: Shape.Choice*): Shape.Fields =
      Shape.Fields(fields.toList)

    def tok(token: Token, tokens: Token*): Shape.Choice =
      Shape.Choice(Set(tokens*).incl(token))
end Wellformed
