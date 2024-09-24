package distcompiler

import cats.syntax.all.given
import distcompiler.Builtin.Error

final class Wellformed private (wfTop: Node.Top, topPattern: Pattern[Unit]):
  lazy val rules: Manip[Unit] =
    import dsl.*
    val goodPattern = Wellformed.rulesBuilder.perform(wfTop.clone())
    pass()
      .rules:
        on(
          theTop <* children(not(topPattern))
        ).value.effect: top =>
          val unparentedChildren = top.unparentedChildren
          val childrenTuple = Builtin.Tuple(unparentedChildren)
          top.children = List(
            Error(
              "unexpected shape at root",
              childrenTuple
            )
          )
          (childrenTuple.firstChild, childrenTuple.firstChild).pure
        | on(
          anyChild <* not(goodPattern)
        ).rewrite: child =>
          Splice(
            Error(
              "unexpected shape or token",
              child.unparent()
            )
          )
  end rules

  def makeDerived(fn: Wellformed.Builder ?=> Unit): Wellformed =
    val builder =
      Wellformed.Builder(wfTop.clone(), topPatternOpt = Some(topPattern))
    fn(using builder)
    builder.build()
end Wellformed

object Wellformed:
  def apply(fn: Builder ?=> Unit): Wellformed =
    val builder = Builder()
    fn(using builder)
    builder.build()

  private val rulesBuilder: Manip[Pattern[Unit]] =
    given NodeMeta[Pattern[Unit]] = NodeMeta.byToString()
    import dsl.*

    val defnPattern: Pattern[(Token, Shape)] =
      tok(Defn)
      *>: children:
        (tok(Name) *>: onlyChild(embedValue[Token]))
          **: embedValue[Shape]

    pass()
      .rules:
        on(
          defnPattern :* rightSibling(embedValue[Pattern[Unit]])
        ).rewrite: (token, shape, restPattern) =>
          Splice(
            Node.Embed((tok(token) *>: children(shape.pattern)) | restPattern)
          )
        | on(
          defnPattern <*: atEnd
        ).rewrite: (token, shape) =>
          Splice(Node.Embed(tok(token) *>: children(shape.pattern)))
        | on(
          tok(Namespace) *> onlyChild(embedValue[Pattern[Unit]])
        ).rewrite: pattern =>
          Splice(Node.Embed(pattern))
    *> on(
      theTop *> onlyChild(embedValue[Pattern[Unit]])
    ).value
  end rulesBuilder

  object Namespace extends Token:
    override def symbolTableFor: List[Token] = List(Wellformed.Name)

  object Defn extends Token:
    override val lookedUpBy: Pattern[Node] =
      import dsl.*
      tok(Defn) *> children:
        find(tok(Wellformed.Name))

  object Name extends Token

  final class Builder private[Wellformed] (
      top: Node.Top = Node.Top(List(Namespace())),
      private var topPatternOpt: Option[Pattern[Unit]] = None
  ):
    private val ns = top(Namespace)

    private def mkDefn(token: Token, pattern: Pattern[?]): Node =
      Defn(
        Name().at(Source(token.name).range),
        Node.Embed(Shape(pattern.void))
      )

    extension (top: Node.Top.type)
      def ::=(pattern: Pattern[?]): Unit =
        require(topPatternOpt.isEmpty)
        topPatternOpt = Some(pattern.void)
      def ::=!(pattern: Pattern[?]): Unit =
        require(topPatternOpt.nonEmpty)
        topPatternOpt = Some(pattern.void)

    extension (token: Token)
      def ::=(pattern: Pattern[?]): Unit =
        val toInsert = mkDefn(token, pattern)
        val name = toInsert(Name)
        require(name.lookupRelativeTo(ns).isEmpty)
        ns.children.addOne(toInsert)

      def ::=!(pattern: Pattern[?]): Unit =
        val toInsert = mkDefn(token, pattern)
        val name = toInsert(Name)
        val targets = name.lookupRelativeTo(ns)
        require(targets.size == 1)
        val List(target) = targets
        // just replace the shape; might make lookups more efficient (eventually)
        target(Shape).replaceThis(toInsert(Shape))

    private[Wellformed] def build(): Wellformed =
      require(topPatternOpt.nonEmpty)
      new Wellformed(top, topPattern = topPatternOpt.get)
  end Builder

  final class Shape(val pattern: Pattern[Unit])

  object Shape extends Token:
    given meta: NodeMeta[Shape] = NodeMeta.byToString()
end Wellformed
