package distcompiler

final class Reader(initState: Node.Top => Reader.State):
  def apply(range: SourceRange): Node.Top =
    val top: Node.Top = Node.Top()

    var currRange = range
    var state: Reader.State = initState(top)
    var shouldContinue = true

    while shouldContinue
    do
      currRange match
        case state(action) =>
          action match
            case Reader.Action.Move(nextRange, nextState) =>
              // simple sanity check to fail on obvious loops
              assert((currRange ne nextRange) || nextState.nonEmpty)
              currRange = nextRange
              nextState match
                case None =>
                case Some(nextState) =>
                  state = nextState
            case Reader.Action.Done =>
              shouldContinue = false
        case _ =>
          top.children.addOne:
            Builtin.Error(
              "invalid",
              Builtin
                .SourceMarker()
                .at:
                  if currRange.isEmpty
                  then currRange
                  else currRange.take(1)
            )
          if currRange.isEmpty
          then shouldContinue = false
          else currRange = currRange.drop(1)
    end while

    top
end Reader

object Reader:
  inline def top(using top: Node.Top): Node.Top = top

  type State = PartialFunction[SourceRange, Reader.Action]

  enum Action:
    case Move(nextRange: SourceRange, nextState: Option[State])
    case Done
  end Action
end Reader
