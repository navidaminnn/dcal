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
    val acc = mutable.HashMap[List[String], mutable.ListBuffer[Token]]()
    acc(Nil) = assigns.iterator.map(_._1).to(mutable.ListBuffer)

    def namesToFix: List[List[String]] =
      acc
        .iterator
        .collect:
          case (name, buf) if buf.size > 1 => name
        .toList

    @scala.annotation.tailrec
    def impl(): Unit =
      val toFix = namesToFix
      if toFix.nonEmpty
      then
        toFix.foreach: nameTooShort =>
          val toks = acc(nameTooShort)
          acc.remove(nameTooShort)

          var didntGrowCount = 0
          toks.foreach: token =>
            val longerName = token.nameSegments.takeRight(nameTooShort.size + 1)
            if longerName == nameTooShort
            then
              assert(didntGrowCount == 0,
                s"duplicate name $nameTooShort in group ${toks.iterator.map(_.name).mkString(", ")}")
              didntGrowCount += 1

            val buf = acc.getOrElseUpdate(longerName, mutable.ListBuffer.empty)
            buf += token

        impl()

    impl()
    acc
      .iterator
      .map: (parts, unitList) =>
        unitList.head -> SourceRange.entire(Source.fromString(parts.mkString(".")))
      .toMap

  private lazy val tokenByShortName: Map[SourceRange, Token] =
    shortNameByToken.map(_.reverse)

  lazy val markErrors: Manip[Unit] =
    // target shapes differently... 
    // can't be "at end" anymore; you have to see it "from above"
    // ... so we assert shapes at parents, with a tiny bit of extra work
    def shapePattern(shape: Wellformed.Shape): Pattern[Node.Child] =
      def forChoice(choice: Shape.Choice): Pattern[Node.Child] =
        oneOfToks(choice.choices)

      ???
      // shape match
      //   case Shape.Atom =>
      //     atEnd
      //   case Shape.AnyShape =>
      //     anySibling
      //   case choice: Shape.Choice =>
      //     forChoice(choice).filter(_.idxInParent == 0)
      //     | atEnd
      //   case Shape.Repeat(choice, minCount) =>
      //     forChoice(choice)
      //     | atEnd.restrict:
      //       case sentinel if sentinel.idxInParent >= minCount => sentinel
      //   case Shape.Fields(fields) =>
      //     val expectedEndIdx = fields.size
      //     val endPat =
      //       atEnd.restrict:
      //         case sentinel if sentinel.idxInParent == expectedEndIdx =>
      //           sentinel

      //     fields.iterator.zipWithIndex
      //       .map: (field, idx) =>
      //         forChoice(field).restrict:
      //           case sibling if sibling.idxInParent == idx => sibling
      //       .foldLeft(endPat: Pattern[Node.Sibling])(_ | _)

    pass(once = true)
      .rules:
        on(
          tok(Builtin.Error)
        ).rewrite(_ => skipMatch(continuePass))
        | on(
          anyChild
          <* not:
            assigns.foldLeft(shapePattern(topShape) <* parent(theTop)):
              case (acc, (token, shape)) =>
                acc
                  | (shapePattern(shape) <* parent(tok(token)))
        ).rewrite: node =>
          splice(
            Error(
              "unexpected shape",
              node.unparent()
            )
          )
  end markErrors

  def makeDerived(fn: Wellformed.Builder ?=> Unit): Wellformed =
    val builder =
      Wellformed.Builder(mutable.HashMap.from(assigns), Some(topShape))
    fn(using builder)
    builder.build()

  lazy val serializeTree: Manip[Unit] =
    extension (node: Node)
      def shortName =
        shortNameByToken.getOrElse(
          node.token,
          SourceRange.entire(Source.fromString(node.token.name))
        )

    pass(once = true)
      .rules:
        on(
          anyAtom
            .filter(n => !n.token.showSource)
        ).rewrite: node =>
          splice(sexpr.tokens.Atom(node.shortName))
        | on(
          anyNode
        ).rewrite: node =>
          val toSkip = mutable.ListBuffer(sexpr.tokens.Atom(node.shortName))
          if node.token.showSource
          then
            toSkip.addOne(sexpr.tokens.Atom("::src"))
            toSkip.addOne:
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
          spliceThen(list):
            Manip.pure(toSkip.last).here(continuePassAtNext)

  lazy val deserializeTree: Manip[Unit] =
    import sexpr.tokens.Atom
    val src = SourceRange.entire(Source.fromString("::src"))
    object srcMapRef extends Manip.Ref[mutable.HashMap[os.Path, Source]]
    pass(once = true)
      .withState(srcMapRef)(mutable.HashMap.empty)
      .rules:
        on(
          tok(Atom)
        ).rewrite: atom =>
          tokenByShortName.get(atom.sourceRange) match
            case None =>
              splice(
                Builtin.Error(
                  s"unknown token \"${atom.sourceRange.decodeString()}\"",
                  atom.unparent()
                )
              )
            case Some(token) =>
              splice(token(atom.sourceRange))
        | on(
          tok(sexpr.tokens.List)
          *> children:
            Fields()
              .field(tok(Atom).map(_.sourceRange).restrict(tokenByShortName))
              .skip(tok(Atom).filter(_.sourceRange == src))
              .field(tok(Atom).map(_.sourceRange))
              .field(repeated(anyChild))
              .pattern
        ).rewrite: (tok, src, children) =>
          children.foreach(_.unparent())
          splice(tok(children).at(src))
        | on(
          tok(sexpr.tokens.List)
          *> children:
            Fields()
              .field:
                tok(Atom)
                  .map(_.sourceRange)
                  .restrict(tokenByShortName)
              .skip(tok(Atom).filter(_.sourceRange == src))
              .field:
                tok(sexpr.tokens.List)
                *> children:
                  Fields()
                    .field(tok(Atom))
                    .field(tok(Atom))
                    .field(tok(Atom))
                    .atEnd
              .field(repeated(anyChild))
              .pattern
        ).rewrite: (tok, bounds, children) =>
          val (orig, offset, len) = bounds
          val path = os.Path(orig.sourceRange.decodeString())
          val offsetInt = offset.sourceRange.decodeString().toInt
          val lenInt = len.sourceRange.decodeString().toInt
          srcMapRef.get.flatMap: srcMap =>
            val src = srcMap.getOrElseUpdate(path, Source.mapFromFile(path))
            children.foreach(_.unparent())
            spliceThen(
              tok(children).at(SourceRange(src, offsetInt, lenInt))
            )(continuePassAtNext)
        | on(
          tok(sexpr.tokens.List)
          *> children:
            Fields()
              .field:
                tok(Atom)
                  .map(_.sourceRange)
                  .restrict(tokenByShortName)
              .field(repeated(anyChild))
              .pattern
        ).rewrite: (tok, children) =>
          children.foreach(_.unparent())
          spliceThen(tok(children))(continuePassAtNext)
        | on(
          tok(sexpr.tokens.List)
            *: firstChild(
              tok(Atom)
                .map(_.sourceRange)
                .filter(src => !tokenByShortName.contains(src))
            )
        ).rewrite: (node, badSrc) =>
          splice(
            Builtin.Error(
              s"could not recognize token name ${badSrc.decodeString()}",
              node.unparent()
            )
          )
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
    case Atom, AnyShape
    case Choice(choices: Set[Token])
    case Repeat(choice: Shape.Choice, minCount: Int)
    case Fields(fields: List[Shape.Choice])

  object Shape:
    extension (choice: Shape.Choice)
      @scala.annotation.targetName("combineChoices")
      def |(other: Shape.Choice): Shape.Choice =
        Shape.Choice(choice.choices ++ other.choices)

    def repeated(choice: Shape.Choice, minCount: Int = 0): Shape.Repeat =
      Shape.Repeat(choice, minCount)

    def fields(fields: Shape.Choice*): Shape.Fields =
      Shape.Fields(fields.toList)

    def tok(token: Token, tokens: Token*): Shape.Choice =
      Shape.Choice(Set(tokens*).incl(token))
end Wellformed
