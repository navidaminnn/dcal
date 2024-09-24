package distcompiler

import cats.syntax.all.given
import distcompiler.Builtin.Error

final class Wellformed private (wfTop: Node.Top, topPattern: Pattern[Unit]):
  lazy val rules: Manip[Unit] =
    val goodPattern = Wellformed.rulesBuilder.perform(wfTop.clone())

    import dsl.*

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
      onlyChild(embedValue[Pattern[Unit]])
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
    private val ns = top.children.head.asNode

    extension (top: Node.Top.type)
      def ::=(pattern: Pattern[?]): Unit =
        topPatternOpt = Some(pattern.void)

    extension (tok: Token)
      def ::=(pattern: Pattern[?]): Unit =
        ns.children.addOne(
          Defn(
            Name().at(Source(tok.name).range),
            Node.Embed(Shape(pattern.void))
          )
        )

    private[Wellformed] def build(): Wellformed =
      require(topPatternOpt.nonEmpty)
      new Wellformed(top, topPattern = topPatternOpt.get)

  final class Shape(val pattern: Pattern[Unit])

  object Shape extends Token:
    given meta: NodeMeta[Shape] with
      // TODO: best-effort rendering from pattern to Node.
      // For most straightforward patterns this will work. For some it will not, and that's ok.
      extension (self: Shape) def asNode: Node = ???
      def doClone(self: Shape): Shape = self
end Wellformed
