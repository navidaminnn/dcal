package distcompiler

import scala.collection.mutable

enum Pattern[+T]:
  case ThisToken(token: Token) extends Pattern[Node]
  case Choose[T](first: Pattern[T], second: Pattern[T]) extends Pattern[T]
  case Adjacent[T1, T2](first: Pattern[T1], second: Pattern[T2])
      extends Pattern[(T1, T2)]
  case And[T1, T2](first: Pattern[T1], second: Pattern[T2])
      extends Pattern[(T1, T2)]

  // subsumes all of parent, child, lookup, lookdown, etc
  // e.g., for looking at parent, you pick Any, you filter .hasParent, and you use (.parent, .indexInParent)
  // ... same for any other transformation where you look at a node, somehow find more nodes, and want to match on them
  case ForThis[T](srcPattern: Pattern[Node.Sibling], pattern: Pattern[T])
      extends Pattern[T]
  case ForAny[T](
      srcPattern: Pattern[IterableOnce[Node.Sibling]],
      pattern: Pattern[T]
  ) extends Pattern[List[T]]
  case ForAll[T](
      srcPattern: Pattern[IterableOnce[Node.Sibling]],
      pattern: Pattern[T]
  ) extends Pattern[List[T]]

  case AnyToken extends Pattern[Node]
  case End extends Pattern[Unit]
  case Repeated[T](pattern: Pattern[T]) extends Pattern[List[T]]

  case Find[T](pattern: Pattern[T]) extends Pattern[T]

  case Lazy[T](patternFn: () => Pattern[T]) extends Pattern[T]

  case Map[T1, T2](pattern: Pattern[T1], fn: T1 => T2) extends Pattern[T2]
  case Filter[T](pattern: Pattern[T], pred: T => Boolean) extends Pattern[T]

  case Negated(pattern: Pattern[?]) extends Pattern[Unit]

  @scala.annotation.alpha("or")
  def |[U >: T](other: Pattern[U]): Pattern[U] =
    Choose(this, other)

  def and[U](other: Pattern[U]): Pattern[(T, U)] =
    And(this, other)

  def nextTo[U](other: Pattern[U]): Pattern[(T, U)] =
    Adjacent(this, other)

  def map[U](fn: T => U): Pattern[U] =
    Map(this, fn)

  def filter(pred: T => Boolean): Pattern[T] =
    Filter(this, pred)

  def inspect[U](pattern: Pattern[U])(using ev: T <:< Node.Child): Pattern[U] =
    ForThis(this.map(ev), pattern)

  def children[U](using T <:< Node.Parent)(pattern: Pattern[U]): Pattern[U] =
    ForThis(this.map(_.firstChild), pattern)

  def parent[U](using T <:< Node.Sibling)(pattern: Pattern[U]): Pattern[U] =
    ForAny(
      this.map: sibling =>
        sibling.parent match
          case parentNode: Node => Iterator.single(parentNode)
          case _: Node.Root     => Iterator.empty
      ,
      pattern
    )
      .filter(_.nonEmpty)
      .map(_.head)

  def ancestors[U](using
      T <:< Node.Sibling
  )(pattern: Pattern[U]): Pattern[List[U]] =
    ForAny(
      this.map(_.parents.collect:
        case parentNode: Node => parentNode
      ),
      pattern
    )

  def firstAncestor[U](using T <:< Node.Sibling)(
      pattern: Pattern[U]
  ): Pattern[U] =
    ancestors(pattern).map(_.head)

  def apply(sibling: Node.Sibling): Pattern.Result[T] =
    import Pattern.Result
    import Result.*

    def impl[T](self: Pattern[T], sibling: Node.Sibling): Pattern.Result[T] =
      def withChild[T](fn: Node.Child => Result[T]): Result[T] =
        sibling match
          case _: Node.RightSiblingSentinel => Rejected
          case child: Node.Child            => fn(child)

      def withNode[T](fn: Node => Result[T]): Result[T] =
        withChild: child =>
          child match
            case node: Node   => fn(node)
            case _: Node.Leaf => Rejected

      self match
        case ThisToken(token) =>
          withNode: node =>
            if node.token == token
            then Matched(node.rightSibling, node)
            else Rejected
        case Choose(first, second) =>
          impl(first, sibling) match
            case m @ Matched(_, _) => m
            case Rejected =>
              impl(second, sibling)

        case Adjacent(first, second) =>
          impl(first, sibling) match
            case Matched(nextSibling, bound1) =>
              impl(second, nextSibling) match
                case Matched(nextSibling2, bound2) =>
                  Matched(nextSibling2, (bound1, bound2))
                case Rejected =>
                  Rejected
            case Rejected => Rejected

        case And(first, second) =>
          impl(first, sibling) match
            case Matched(nextSibling, bound1) =>
              impl(second, sibling) match
                case Matched(nextSibling2, bound2) =>
                  val maxNextSibling =
                    if nextSibling.idxInParent < nextSibling2.idxInParent
                    then nextSibling2
                    else nextSibling
                  Matched(maxNextSibling, (bound1, bound2))
                case Rejected => Rejected

            case Rejected => Rejected

        case ForThis(srcPattern, pattern) =>
          impl(srcPattern, sibling) match
            case Matched(nextSibling, siblingToMatch) =>
              impl(pattern, siblingToMatch) match
                case Matched(_, bound) => Matched(nextSibling, bound)
                case Rejected          => Rejected
            case Rejected => Rejected

        case ForAny(srcPattern, pattern) =>
          impl(srcPattern, sibling) match
            case Matched(nextSibling, siblingsToMatch) =>
              val elems =
                siblingsToMatch.iterator
                  .map(impl(pattern, _))
                  .collect:
                    case Matched(_, bound) => bound
                  .toList
              Matched(nextSibling, elems)
            case Rejected => Rejected

        case ForAll(srcPattern, pattern: Pattern[t]) =>
          impl(srcPattern, sibling) match
            case Matched(nextSibling, siblingsToMatch) =>
              val elems =
                siblingsToMatch.iterator
                  .map(impl(pattern, _))
                  .toList

              if elems.forall:
                  case _: Matched[t] => true
                  case Rejected      => false
              then
                Matched(
                  nextSibling,
                  elems.collect { case Matched(_, bound) => bound }
                )
              else Rejected
            case Rejected => Rejected

        case AnyToken =>
          withNode(node => Matched(node.rightSibling, node))
        case End =>
          sibling match
            case _: Node.RightSiblingSentinel => Matched(sibling, ())
            case _: Node.Child                => Rejected
        case Repeated(pattern: Pattern[t]) =>
          val buf = mutable.ListBuffer.empty[t]
          var lastSibling = sibling

          @scala.annotation.tailrec
          def repeatedImpl(sibling: Node.Sibling): Result[T] =
            impl(pattern, sibling) match
              case Matched(nextSibling, bound) =>
                buf += bound
                assert(
                  nextSibling ne lastSibling
                ) // or we cause an infinite loop
                lastSibling = nextSibling
                repeatedImpl(nextSibling)
              case Rejected =>
                Matched(sibling, buf.toList)

          repeatedImpl(sibling)

        case Find(pattern: Pattern[t]) =>
          @scala.annotation.tailrec
          def findImpl(sibling: Node.Sibling): Result[t] =
            impl(pattern, sibling) match
              case m: Matched[t] => m
              case Rejected =>
                if !sibling.isChild // means we are trying to go right from a sentinel
                then Rejected
                else findImpl(sibling.rightSibling)

          findImpl(sibling)

        case Lazy(patternFn)  => impl(patternFn(), sibling)
        case Map(pattern, fn) => impl(pattern, sibling).map(fn)
        case Filter(pattern, pred) =>
          impl(pattern, sibling) match
            case m @ Matched(_, bound) =>
              if pred(bound)
              then m
              else Rejected
            case Rejected => Rejected

        case Negated(pattern) =>
          impl(pattern, sibling) match
            case Matched(_, _) => Rejected
            case Rejected      => Matched(sibling, ())

    impl(this, sibling)
end Pattern

object Pattern:
  def tok(token: Token): Pattern[Node] =
    ThisToken(token)

  def find[T](pattern: Pattern[T]): Pattern[T] =
    Find(pattern)

  given adjacentTuple[PatternTpl <: NonEmptyTuple]
      : Conversion[PatternTpl, Pattern[Tuple.InverseMap[PatternTpl, Pattern]]]
  with
    def apply(tpl: PatternTpl): Pattern[Tuple.InverseMap[PatternTpl, Pattern]] =
      tpl.productIterator
        .asInstanceOf[Iterator[Pattern[Any]]]
        .foldLeft(None: Option[Pattern[NonEmptyTuple]]): (acc, elem) =>
          acc match
            case None             => Some(elem.map(Tuple1.apply))
            case Some(accPattern) => Some((accPattern.nextTo(elem)).map(_ :* _))
        .get
        .asInstanceOf

  enum Result[+T]:
    case Matched[T](nextSibling: Node.Sibling, bound: T) extends Result[T]
    case Rejected

    def map[U](fn: T => U): Result[U] =
      this match
        case Matched(nextSibling, bound) => Matched(nextSibling, fn(bound))
        case Rejected                    => Rejected

  end Result
end Pattern
