package distcompiler

import cats.syntax.all.given
import scala.collection.mutable
import Builtin.Error

final class Wellformed private (
    assigns: Map[Token, Wellformed.Shape],
    topShape: Wellformed.Shape
):
  require:
    assigns.iterator.map(_._1.name).distinct.size == assigns.size
  import Wellformed.Shape
  import dsl.*

  private lazy val shortNameByToken: Map[Token, SourceRange] =
    val tokenNameSegments = assigns.iterator
      .map: (token, _) =>
        token.name.split('.') -> token
      .toArray

    def impl(
        tokenNameSegments: Array[(Array[String], Token)]
    ): Map[Token, String] =
      tokenNameSegments
        .groupBy(_._1.last)
        .iterator
        .flatMap:
          case (shortName, Array((_, tok))) =>
            Iterator.single(tok -> shortName)
          case (dupName, arr) =>
            impl:
              arr.mapInPlace: (segments, tok) =>
                segments.init -> tok
            .map: (tok, name) =>
              tok -> s"$name.$dupName"
        .toMap

    impl(tokenNameSegments)
      .map: (tok, name) =>
        tok -> SourceRange.entire(Source.fromString(name))

  private lazy val tokenByShortName: Map[SourceRange, Token] =
    shortNameByToken.map(_.reverse)

  // private lazy val tokensByName: Map[SourceRange, Token] =
  //   assigns.map: (token, _) =>
  //     SourceRange.entire(Source.fromString(token.name)) -> token

  // private lazy val namesByToken: Map[Token, SourceRange] =
  //   assigns.map: (token, _) =>
  //     token -> SourceRange.entire(Source.fromString(token.name))

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
            .getOrElse(Pattern.reject)

    pass(once = true)
      .rules:
        on(
          anySibling
          <* not:
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

  // def checkTree(top: Node.Top): Unit =
  //   markErrors.perform(top)

  // private lazy val rewriteFromSExpressions: Manip[Unit] =
  //   pass(strategy = pass.bottomUp, once = true)
  //     .rules:
  //       on(
  //         tok(sexpr.tokens.List).map(_.sourceRange)
  //         *: children:
  //           tok(sexpr.tokens.Atom)
  //             .map(_.sourceRange)
  //             .restrict(tokensByName)
  //             *: repeated(anyChild)
  //       ).rewrite: (src, token, listOfChildren) =>
  //         Splice(token(listOfChildren.iterator.map(_.unparent())).at(src))

  // def fromSExpression(top: Node.Top): Unit =
  //   locally:
  //     sexpr.wellFormed.markErrors
  //       *> rewriteFromSExpressions
  //       *> markErrors
  //   .perform(top)

  // private lazy val rewriteToSExpressions: Manip[Unit] =
  //   pass(once = true)
  //     .rules:
  //       on(
  //         anyNode
  //           *: anyNode
  //             .map(_.token)
  //             .restrict(namesByToken)
  //       ).rewrite: (node, name) =>
  //         val list = sexpr.tokens.List()
  //         list.children.addOne(sexpr.tokens.Atom(name))
  //         list.children.addAll(node.unparentedChildren)
  //         Splice(list)

  // def toSExpression(top: Node.Top): Unit =
  //   locally:
  //     markErrors
  //       *> rewriteToSExpressions
  //       *> sexpr.wellFormed.markErrors
  //   .perform(top)

  lazy val serializeTree: Manip[Unit] =
    extension (node: Node)
      def shortName =
        shortNameByToken.getOrElse(
          node.token,
          SourceRange.entire(Source.fromString(node.token.name))
        )

    markErrors
    *> pass(once = true)
      .rules:
        on(
          tok(Wellformed.SkipMarker)
        ).rewrite(_ => Skip)
        | on(
          anyNode
            .filter(n => !n.token.showSource)
            <* children(atEnd)
        ).rewrite: node =>
          Splice(sexpr.tokens.Atom(node.shortName))
        | on(
          anyNode
        ).rewrite: node =>
          val toSkip = Wellformed.SkipMarker(sexpr.tokens.Atom(node.shortName))
          if node.token.showSource
          then
            toSkip.children.addOne(sexpr.tokens.Atom("::src"))
            toSkip.children.addOne:
              val src = node.sourceRange
              src.source.origin match
                case None =>
                  sexpr.tokens.Atom(src)
                case Some(origin) =>
                  sexpr.tokens.List(
                    sexpr.tokens.Atom(origin.toString),
                    sexpr.tokens.Atom(src.offset.toString()),
                    sexpr.tokens.Atom(src.length.toString())
                  )
          val list = sexpr.tokens.List(toSkip)
          list.children.addAll(node.unparentedChildren)
          Splice(list)
    *> pass(once = true)
      .rules:
        on(
          tok(Wellformed.SkipMarker)
        ).rewrite: marker =>
          Splice(marker.unparentedChildren.iterator.toSeq*)

  lazy val deserializeTree: Manip[Unit] =
    import sexpr.tokens.Atom
    val src = SourceRange.entire(Source.fromString("::src"))
    pass(once = true)
      .rules:
        on(
          tok(Atom)
        ).rewrite: atom =>
          tokenByShortName.get(atom.sourceRange) match
            case None =>
              Splice(
                Builtin.Error(s"unknown token \"${atom.sourceRange}\"", atom)
              )
            case Some(token) =>
              Splice(token(atom.sourceRange))
        | on(
          tok(sexpr.tokens.List)
          *>: children:
            tok(Atom)
            *>:
              locally[Pattern[Either[List[Node.Child], Either[
                (Node, List[Node.Child]),
                ((Node, Node, Node), List[Node.Child])
              ]]]]:
                tok(Atom).filter(_.sourceRange == src)
                  *>:
                    locally:
                      (tok(Atom) **: repeated(anyChild)).map(Left(_))
                        | locally:
                          (tok(sexpr.tokens.List)
                          *> children:
                            tok(Atom) **: tok(Atom) **: tok(Atom) <*: atEnd
                          )
                            **: repeated(anyChild)
                        .map(Right(_))
                    .map(Right(_))
                    | repeated(anyChild).map(Left(_))
        ).rewrite: result =>
          ???
        | on(
          tok(sexpr.tokens.List)
          *> children:
            tok(Atom).map(_.sourceRange).restrict(tokenByShortName)
              **: (tok(Atom).filter(_.sourceRange == src)
                *>: tok(Atom).map(_.sourceRange)
                **: repeated(anyChild))
        ).rewrite: (tok, src, children) =>
          Splice(tok(children.map(_.unparent())).at(src))
        | on(
          tok(sexpr.tokens.List)
          *> children:
            tok(Atom).map(_.sourceRange).restrict(tokenByShortName)
              **: (tok(Atom).filter(_.sourceRange == src)
                *>: (tok(sexpr.tokens.List)
                *> children:
                  tok(Atom) **: tok(Atom) **: tok(Atom) <*: atEnd
                )
                **: repeated(anyChild))
        ).rewrite: (tok, bounds, children) =>
          val (orig, offset, len) = bounds
          val path = os.Path(orig.sourceRange.decodeString())
          // TODO: actually merge the source files so they don't get mapped N times
          // probably make rewrite support Manip and use state to store a map
          // ... now I think about it, why not just pass the pattern match result via state too
          ???

    ???
end Wellformed

object Wellformed:
  private object SkipMarker extends Token

  def apply(fn: Builder ?=> Unit): Wellformed =
    val builder = Builder(mutable.HashMap.empty, None)
    fn(using builder)
    builder.build()

  final class Builder private[Wellformed] (
      assigns: mutable.HashMap[Token, Wellformed.Shape],
      private var topShapeOpt: Option[Wellformed.Shape]
  ):
    extension (top: Node.Top.type)
      @scala.annotation.targetName("setTopShape")
      def ::=(shape: Shape): Unit =
        require(topShapeOpt.isEmpty)
        topShapeOpt = Some(shape)
      @scala.annotation.targetName("resetTopShape")
      def ::=!(shape: Shape): Unit =
        require(topShapeOpt.nonEmpty)
        topShapeOpt = Some(shape)

    extension (token: Token)
      @scala.annotation.targetName("setShape")
      def ::=(shape: Shape): Unit =
        require(!assigns.contains(token))
        assigns(token) = shape

      @scala.annotation.targetName("resetShape")
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
      @scala.annotation.targetName("combineChoices")
      def |(other: Shape.Choice): Shape.Choice =
        Shape.Choice(choice.choices ++ other.choices)

    def repeated(choice: Shape.Choice): Shape.Repeat =
      Shape.Repeat(choice)

    def fields(fields: Shape.Choice*): Shape.Fields =
      Shape.Fields(fields.toList)

    def tok(token: Token, tokens: Token*): Shape.Choice =
      Shape.Choice(Set(tokens*).incl(token))
end Wellformed
