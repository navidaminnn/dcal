package distcompiler

import cats.syntax.all.given
import distcompiler.Builtin.Error

final class Wellformed private (wfTop: Node.Top, topPattern: Pattern[Unit]):
  private val ns = wfTop.children.head.asNode

  lazy val rules: Manip[Unit] =
    import Manip.*
    pass()
      .rules:
        import Pattern.*
        on(
          locally:
            anyTok
            *: anyTok
              .map: thisNode =>
                Wellformed.Name(Node.Embed(thisNode.token))
              .here:
                refersToRelative(pure(ns)):
                  tok(Wellformed.Defn)
                  *> children:
                    find:
                      tok(Wellformed.Shape)
                      *> onlyChild:
                        embedValue[Wellformed.Shape]
          .flatMap: (node, shape) =>
            pure(node).here(shape.pattern)
        ).rewrite:
          case () => Skip
        | on(embed[Any]).rewrite(_ => Skip)
        | on(anyChild)
          .rewrite: child =>
            Splice(Error(
              "unexpected shape",
              child.unparent(),
            ))

  def makeDerived(fn: Wellformed.Builder ?=> Unit): Wellformed =
    val builder = Wellformed.Builder(wfTop.clone(), topPatternOpt = Some(topPattern))
    fn(using builder)
    builder.build()
end Wellformed

object Wellformed:
  def apply(fn: Builder ?=> Unit): Wellformed =
    val builder = Builder()
    fn(using builder)
    builder.build()

  object Namespace extends Token:
    override def symbolTableFor: List[Token] = List(Wellformed.Name)

  object Defn extends Token:
    override val lookedUpBy: Pattern[Node] =
      import Pattern.*
      tok(Defn) *> children:
        find(tok(Wellformed.Name))

  object Name extends Token

  final class Builder private[Wellformed] (top: Node.Top = Node.Top(List(Namespace())), private var topPatternOpt: Option[Pattern[Unit]] = None):
    private val ns = top.children.head.asNode

    extension (top: Node.Top.type) def ::=(pattern: Pattern[?]): Unit =
      topPatternOpt = Some(pattern.void)

    extension (tok: Token) def ::=(pattern: Pattern[?]): Unit =
      ns.children.addOne(Defn(Name().at(Source(tok.name).range), Node.Embed(Shape(pattern.void))))

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
