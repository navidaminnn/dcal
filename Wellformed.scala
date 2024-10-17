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
      acc.iterator
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
              assert(
                didntGrowCount == 0,
                s"duplicate name $nameTooShort in group ${toks.iterator.map(_.name).mkString(", ")}"
              )
              didntGrowCount += 1

            val buf = acc.getOrElseUpdate(longerName, mutable.ListBuffer.empty)
            buf += token

        impl()

    impl()
    acc.iterator
      .filter: (parts, unitList) =>
        if unitList.isEmpty
        then
          assert(parts == Nil)
          false
        else true
      .map: (parts, unitList) =>
        unitList.head -> SourceRange.entire(
          Source.fromString(parts.mkString("."))
        )
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
        ).rewrite(_ => skipMatch)
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

  import scala.util.control.TailCalls.*

  extension [T](self: IterableOnce[T])
    private def flatForEach(fn: T => TailRec[Unit]): TailRec[Unit] =
      val iter = self.iterator
      def impl(): TailRec[Unit] =
        if iter.hasNext
        then tailcall(fn(iter.next())).flatMap(_ => impl())
        else done(())

      impl()

  def serializeTree(tree: Node.All): tree.This =
    import sexpr.tokens.{Atom, List as SList}

    val src = SourceRange.entire(Source.fromString(":src"))
    val txt = SourceRange.entire(Source.fromString(":txt"))

    extension [P <: Node.Parent](parent: P)
      def addSerializedChildren(
          children: IterableOnce[Node.Child]
      ): TailRec[P] =
        children
          .flatForEach: child =>
            tailcall(impl(child)).map(parent.children.addOne)
          .map(_ => parent)

    extension (token: Token)
      def shortName =
        shortNameByToken.getOrElse(
          token,
          SourceRange.entire(Source.fromString(token.name))
        )

    def impl(tree: Node.All): TailRec[tree.This] =
      val result: TailRec[Node.All] =
        tree match
          case top: Node.Top =>
            val result = Node.Top()
            result.addSerializedChildren(top.children)
          case nd @ Node(token) if !token.showSource =>
            done(Atom(token.shortName))
          case node: Node =>
            val result = SList(Atom(node.token.shortName))
            if node.token.showSource
            then
              val srcRange = node.sourceRange
              result.children.addOne:
                SList(
                  Atom(txt),
                  Atom(srcRange)
                )
              srcRange.source.origin match
                case None =>
                case Some(origin) =>
                  result.children.addOne:
                    SList(
                      Atom(src),
                      Atom(origin.toString),
                      Atom(srcRange.offset.toString()),
                      Atom(srcRange.length.toString())
                    )

            result.addSerializedChildren(node.children)
          case _: Node.Embed[?] =>
            ???

      result.asInstanceOf[TailRec[tree.This]]

    impl(tree).result
  end serializeTree

  def deserializeTree(tree: Node.All): Node.All =
    import sexpr.tokens.{Atom, List as SList}

    val src = SourceRange.entire(Source.fromString(":src"))
    val txt = SourceRange.entire(Source.fromString(":txt"))
    val srcMap = mutable.HashMap.empty[os.Path, Source]

    lazy val nodeManip: Manip[TailRec[Node.All]] =
      on(
        tok(Atom)
          .map(_.sourceRange)
          .restrict(tokenByShortName)
          <* children(atEnd)
      ).value.map: token =>
        done(token())
      | on(
        anyNode *:
          tok(SList).withChildren:
            Fields()
              .field:
                tok(Atom)
                  .map(_.sourceRange)
                  .restrict(tokenByShortName)
              .optional:
                tok(SList).withChildren:
                  Fields()
                    .skip(tok(Atom).filterSrc(txt))
                    .field(tok(Atom).map(_.sourceRange))
                    .atEnd
                    .map(_._1)
              .optional:
                tok(SList).withChildren:
                  Fields()
                    .skip(tok(Atom).filterSrc(src))
                    .field(tok(Atom))
                    .field(tok(Atom))
                    .field(tok(Atom))
                    .atEnd
              .trailing
      ).value.map: (node, token, txtOpt, srcOpt) =>
        val result = token()
        srcOpt match
          case None =>
          case Some((path, offset, len)) =>
            val pathVal = os.Path(path.sourceRange.decodeString())
            val offsetVal = offset.sourceRange.decodeString().toInt
            val lenVal = len.sourceRange.decodeString().toInt
            val loc =
              srcMap.getOrElseUpdate(pathVal, Source.mapFromFile(pathVal))
            result.at(SourceRange(loc, offsetVal, lenVal))
        txtOpt match
          case None =>
          case Some(text) if srcOpt.nonEmpty =>
            assert(text == result.sourceRange)
          case Some(text) =>
            result.at(text)

        val skipCount =
          1 + (if srcOpt.nonEmpty then 1 else 0) + (if txtOpt.nonEmpty then 1
                                                    else 0)
        tailcall:
          result
            .addDeserializedChildren(node.children.iterator.drop(skipCount))
            .map(_ => result)
      | on(
        anyNode
      ).value.map: badNode =>
        done(
          Builtin.Error(
            "could not parse node",
            badNode.clone()
          )
        )

    extension [P <: Node.Parent](parent: P)
      def addDeserializedChildren(
          children: IterableOnce[Node.Child]
      ): TailRec[P] =
        children
          .flatForEach: child =>
            tailcall(impl(child)).map(parent.children.addOne)
          .map(_ => parent)

    def impl[N <: Node.All](tree: N): TailRec[N] =
      (tree: @unchecked) match
        case top: Node.Top =>
          val result: Node.Top = Node.Top()
          result
            .addDeserializedChildren(top.children)
            .map(_ => result.asInstanceOf[N])
        case node: Node =>
          atNode(node)(nodeManip)
            .perform()
            .asInstanceOf[TailRec[N]]

    impl(tree).result
  end deserializeTree
end Wellformed

object Wellformed:
  val empty: Wellformed = Wellformed:
    Node.Top ::= Wellformed.Shape.AnyShape

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
