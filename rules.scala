package distcompiler

import scala.collection.mutable

final class Rule[T](guard: Pattern[T], action: Rule.Action[T]):
  def apply(sibling: Node.Sibling): Rule.Result =
    ???
  // guard(sibling) match
  //   case Pattern.Result.Matched(nextSibling, bound) =>
  //     val spanLength = nextSibling.idxInParent - sibling.idxInParent

  //     action(bound) match
  //       case replacementNode: Node =>
  //         sibling.parent.children.patchInPlace(
  //           sibling.idxInParent,
  //           Iterator.single(replacementNode),
  //           spanLength
  //         )
  //         Rule.Result.Progress(replacementNode)
  //       case Rule.Splice(nodes*) =>
  //         val parent = sibling.parent
  //         val idxInParent = sibling.idxInParent
  //         parent.children.patchInPlace(idxInParent, nodes, spanLength)
  //         Rule.Result.Progress(parent.children.findSibling(idxInParent))
  //       case Rule.Empty =>
  //         val parent = sibling.parent
  //         val idxInParent = sibling.idxInParent
  //         parent.children.patchInPlace(
  //           idxInParent,
  //           Iterator.empty,
  //           spanLength
  //         )
  //         Rule.Result.Progress(parent.children.findSibling(idxInParent))
  //       case Rule.TryNext =>
  //         Rule.Result.TryNext

  //   case Pattern.Result.Rejected =>
  //     Rule.Result.TryNext
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
    case Progress(transformedSibling: Node.Sibling)
end Rule

trait Pass(using NamespaceCtx) extends Named:
  protected given pass: Pass = this
  private val rules = mutable.ArrayBuffer.empty[Rule[?]]

  final def apply(top: Node.Top): Unit =
    def applyRules(sibling: Node.Sibling): Rule.Result =
      rules.iterator
        .map(_.apply(sibling))
        .collectFirst:
          case result: Rule.Result.Progress => result
        .getOrElse:
          Rule.Result.TryNext

    var touched = false

    @scala.annotation.tailrec
    def impl(sibling: Node.Sibling): Unit =
      applyRules(sibling) match
        case Rule.Result.TryNext =>
          if sibling.isChild
          then impl(sibling.rightSibling)
          else
            sibling.parent match
              case _: Node.Root => ()
              case parent: Node =>
                impl(parent.rightSibling)
        case Rule.Result.Progress(transformedSibling) =>
          touched = true

          transformedSibling match
            case transformedNode: Node =>
              impl(transformedNode.firstChild)
            case transformedSentinel: Node.RightSiblingSentinel =>
              transformedSentinel.parent match
                case parentNode: Node =>
                  impl(parentNode.rightSibling)
                case _: Node.Root => ()

            case leaf: Node.Leaf =>
              impl(leaf.rightSibling)

    while
      impl(top.firstChild)
      touched
    do
      touched = false
      if this ne Pass.ResolveBuiltins
      then Pass.ResolveBuiltins(top)
    end while
end Pass

// TODO: make rewrite an algebra too (applicative with monad mode)
// - Trieste's features are just custom operators that do things like "rewrite all matching" etc...
// - rules use disjunction, recursion, etc
// - some consideration for tabling optimizations; at least, have a lazy val that gives you a tabled version of a rule?
// - uses patterns as embeds (auto conversion or smth)
// - rewrite action targeted on a range of nodes (pattern result)
// - navigates tree
// - put marker for when you start over (marks big-scale ops)

case object Pass extends NamespaceObj:
  def rule[T](using pass: Pass)(guard: Pattern[T])(
      action: Rule.Action[T]
  ): Unit =
    pass.rules.addOne(Rule(guard, action))

  final case class Result(node: Node, ok: Boolean)

  import Pattern.*

  case object ResolveBuiltins extends PassObj:
  // rule(
  //   tok(Builtin.lift)
  //     .children:
  //       (tok(Builtin.liftDest), tok(Builtin.liftNode), tok(Builtin.origNode))
  // ):
  //   case (dest, node, orig) =>
  //     val destTok = dest.token

  //     // TODO: ancestor search for token to add the node to

  //     dest.children.patchInPlace(
  //       dest.children.length,
  //       Iterator.single(node.unparent()),
  //       0
  //     )
  //     orig.unparent()
  end ResolveBuiltins
end Pass

trait PassObj extends Pass, NamedObj:
  self: Singleton & Product =>
end PassObj
