package distcompiler

import scala.collection.mutable

final class Rule[T](guard: Pattern[T], action: Rule.Action[T]):
  def apply(parent: Node, startIdx: Int): Rule.Result =
    guard(parent, startIdx) match
      case Pattern.Result.Matched(length, bound) =>
        action(bound) match
          case replacementNode: Node =>
            parent.children.patchInPlace(
              startIdx,
              Iterator.single(replacementNode),
              length
            )
            Rule.Result.Progress(startIdx + 1)
          case Rule.Splice(nodes*) =>
            parent.children.patchInPlace(startIdx, nodes, length)
            Rule.Result.Progress(startIdx + nodes.length)
          case Rule.Empty =>
            parent.children.patchInPlace(startIdx, Iterator.empty, length)
            Rule.Result.Progress(startIdx)
          case Rule.TryNext =>
            Rule.Result.TryNext

      case Pattern.Result.Rejected =>
        Rule.Result.TryNext
end Rule

object Rule:
  type Action[T] = T => ActionResult

  final case class Splice(nodes: Node.Child*)
  case object Empty
  case object TryNext

  type ActionResult =
    Node | Splice | Empty.type | TryNext.type

  enum Result:
    case TryNext
    case Progress(nextUntouchedIdx: Int)
end Rule

trait Pass(using NamespaceCtx) extends Named:
  protected given pass: Pass = this
  private val rules = mutable.ArrayBuffer.empty[Rule[?]]

  final def apply(top: Node): Unit =
    def applyRules(parent: Node, startIdx: Int): Rule.Result =
      rules.iterator
        .map(_.apply(parent, startIdx))
        .collectFirst:
          case result: Rule.Result.Progress => result
        .getOrElse:
          Rule.Result.TryNext

    var touched = false

    @scala.annotation.tailrec
    def impl(parent: Node, startIdx: Int): Unit =
      inline def skipToNextUntouchedSibling(nextUntouchedIdx: Int): Unit =
        if !parent.children.isDefinedAt(startIdx)
        then
          assert(startIdx == parent.children.length)
          if parent.hasParent
          then impl(parent.parent, parent.idxInParent + 1)
          else ()
        else impl(parent, nextUntouchedIdx)

      applyRules(parent, startIdx) match
        case Rule.Result.Progress(nextUntouchedIdx) =>
          // we did something. make note, and leave this subtree alone as it might
          // be unstable. move onto the next one, which might independently do something
          touched = true

          skipToNextUntouchedSibling(nextUntouchedIdx)

        case Rule.Result.TryNext =>
          // inspect children. maybe they match.
          parent.children(startIdx) match
            case child: Node =>
              impl(child, 0)
            case Node.Embed(_) =>
              // unless it's an embed, in which case it cannot be matched
              skipToNextUntouchedSibling(startIdx + 1)

    while
      impl(top, 0)
      touched
    do
      touched = false
      if this ne Pass.ResolveBuiltins
      then Pass.ResolveBuiltins(top)
    end while
end Pass

case object Pass extends NamespaceObj:
  def rule[T](using pass: Pass)(guard: Pattern[T])(
      action: Rule.Action[T]
  ): Unit =
    pass.rules.addOne(Rule(guard, action))

  final case class Result(node: Node, ok: Boolean)

  import Pattern.*

  case object ResolveBuiltins extends PassObj:
    rule(
      tok(Builtin.lift).children:
        (tok(Builtin.liftDest), tok(Builtin.liftNode), tok(Builtin.origNode))
    ):
      case (dest, node, orig) =>
        dest.children.patchInPlace(
          dest.children.length,
          Iterator.single(node.reparent),
          0
        )
        orig.reparent
  end ResolveBuiltins
end Pass

trait PassObj extends Pass, NamedObj:
  self: Singleton & Product =>
end PassObj
