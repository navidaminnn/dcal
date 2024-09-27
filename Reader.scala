package distcompiler

import cats.syntax.all.given

trait Reader:
  protected def rules: Manip[SourceRange]

  final def apply(sourceRange: SourceRange): Node.Top =
    val top = Node.Top()

    val manip =
      Reader.srcRef.init(sourceRange):
        Reader.matchedRef.init(sourceRange.take(0)):
          rules

    manip.perform(top)
    top

object Reader:
  import dsl.*

  private object srcRef extends Manip.Ref[SourceRange]
  private object matchedRef extends Manip.Ref[SourceRange]

  // TODO: other ways to access matched ranges

  def consumeMatch[T](fn: SourceRange => Manip[T]): Manip[T] =
    matchedRef.get.lookahead.flatMap: m =>
      matchedRef.updated(m => m.drop(m.length))(fn(m))

  def dropMatch[T](manip: Manip[T]): Manip[T] =
    matchedRef.updated(m => m.drop(m.length))(manip)

  def extendThisNodeWithMatch[T](using DebugInfo)(manip: Manip[T]): Manip[T] =
    consumeMatch: m =>
      Manip.ThisNode.lookahead.effect:
        case node: Node =>
          node.extendLocation(m)
          manip
        case top: Node.Top =>
          // if top is an extend target, ignore it.
          manip
        case _ => Manip.Backtrack(summon[DebugInfo])

  object bytes:
    private def advancingRefs[T](manip: Manip[T]): Manip[T] =
      srcRef.updated(_.tail):
        matchedRef.updated(_.extendRight):
          manip

    class selecting[T] private (private val map: Map[Byte, Manip[T]])
        extends AnyVal:
      def on(char: Char)(manip: => Manip[T]): selecting[T] =
        on(char.toByte)(manip)

      def on(bytes: IterableOnce[Byte])(manip: => Manip[T]): selecting[T] =
        val impl = defer(manip)
        new selecting(map ++ bytes.iterator.map(_ -> impl))

      def on(byte: Byte)(manip: => Manip[T]): selecting[T] =
        new selecting(map.updated(byte, defer(manip)))

      def fallback(using DebugInfo)(manip: => Manip[T]): Manip[T] =
        lazy val fallbackImpl = manip
        srcRef.get.flatMap: src =>
          if src.isEmpty
          then fallbackImpl
          else
            map.get(src.head) match
              case None => fallbackImpl
              case Some(branch) =>
                advancingRefs(branch)

    object selecting:
      def apply[T]: selecting[T] = new selecting[T](Map.empty)

    def selectManyLike[T](using DebugInfo)(bytes: Set[Byte])(
        manip: Manip[T]
    ): Manip[T] =
      lazy val impl: Manip[T] =
        srcRef.get.lookahead.flatMap: src =>
          if src.nonEmpty && bytes(src.head)
          then advancingRefs(impl)
          else Manip.Backtrack(summon[DebugInfo])
        | manip

      impl

    def selectCount[T](using DebugInfo)(count: Int)(manip: Manip[T]): Manip[T] =
      srcRef.get.lookahead.flatMap: src =>
        if count <= src.length
        then
          srcRef.updated(_.drop(count)):
            matchedRef.updated(_.extendRightBy(count)):
              manip
        else Manip.Backtrack(summon[DebugInfo])

    def getSrc: Manip[SourceRange] =
      srcRef.get
