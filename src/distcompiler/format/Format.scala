package distcompiler.format

import cats.*
import cats.syntax.all.given
import cats.data.Chain

enum Format derives CanEqual {
  import Format.*
  this match {
    case Newline(indentBy) =>
      require(indentBy >= 0)
    case Str(str) =>
      require(!str.contains('\n'))
    case _ =>
  }

  case Empty
  case Newline(indentBy: Int)
  case Str(str: String)
  case Concat(pre: Eval[Format], post: Eval[Format])
  case MultilineAlternative(current: Eval[Format], alternative: Eval[Format])

  val widthInfo: Eval[WidthInfo] =
    this match {
      case Empty =>
        Eval.now(WidthInfo.Single(width = 0))
      case Newline(indentBy) =>
        Eval.now(WidthInfo.Multi(preWidth = 0, maxWidth = 0, postWidth = indentBy))
      case Str(str) =>
        Eval.now(WidthInfo.Single(width = str.size))
      case Concat(pre, post) =>
        (pre.flatMap(_.widthInfo), post.flatMap(_.widthInfo)).mapN {
          case (WidthInfo.Single(preWidth), WidthInfo.Single(postWidth)) =>
            WidthInfo.Single(width = preWidth + postWidth)
          case (WidthInfo.Single(preWidth), postInfo: WidthInfo.Multi) =>
            postInfo.copy(preWidth = preWidth + postInfo.preWidth)
          case (preInfo: WidthInfo.Multi, WidthInfo.Single(postWidth)) =>
            preInfo.copy(postWidth = preInfo.postWidth + postWidth)
          case (preInfo: WidthInfo.Multi, postInfo: WidthInfo.Multi) =>
            WidthInfo.Multi(
              preWidth = preInfo.preWidth,
              maxWidth = preInfo.maxWidth 
                `max` (preInfo.postWidth + postInfo.preWidth)
                `max` postInfo.maxWidth,
              postWidth = postInfo.postWidth,
            )
        }
        .memoize
      case MultilineAlternative(current, alternative) =>
        current.flatMap(_.widthInfo)
    }

  def alignIndentTo[T](fn: Format.IndentCtx ?=> T): T =
    widthInfo
      .map {
        case WidthInfo.Single(width) =>
          fn(using IndentCtx.fromInt(width))
        case WidthInfo.Multi(_, _, postWidth) =>
          fn(using IndentCtx.fromInt(postWidth))
      }
      .value

  def orMultiline(alternative: =>Eval[Format]): Format =
    MultilineAlternative(Eval.now(this), Eval.defer(alternative))

  def ++(other: Format): Format =
    this `combine` other

  def lines(badWidth: Int = 120): List[String] = {
    def impl(format: Format): Eval[Chain[Chain[String]]] =
      format match {
        case Empty => Eval.now(Chain.nil)
        case Newline(indentBy) =>
          Eval.now(Chain(Chain.nil, Chain.one(" " * indentBy)))
        case Str(str) =>
          Eval.now(Chain.one(Chain.one(str)))
        case Concat(pre, post) =>
          (pre.flatMap(impl), post.flatMap(impl)).mapN { (pre, post) =>
            (pre.initLast, post.uncons) match {
              case (None, None) => Chain.nil
              case (None, _) => post
              case (_, None) => pre
              case (Some((preInit, preLast)), Some((postHead, postTail))) =>
                preInit
                ++ Chain.one(preLast ++ postHead)
                ++ postTail
            }
          }
        case MultilineAlternative(current, alternative) =>
          current.flatMap { current =>
            current
              .widthInfo
              .flatMap { currentWidthInfo =>
                if(currentWidthInfo.summarizeMaxWidth > badWidth) {
                  alternative.flatMap(impl)
                } else {
                  impl(current)
                }
              }
          }
      }
      

    impl(this)
      .value
      .iterator
      .map {
        case parts if parts.forall(_.forall(_ == ' ')) => ""
        case parts => parts.iterator.mkString
      }
      .toList
  }
}

object Format {
  enum WidthInfo {
    case Single(width: Int)
    case Multi(preWidth: Int, maxWidth: Int, postWidth: Int)

    def summarizeMaxWidth: Int =
      this match {
        case Single(width) => width
        case Multi(preWidth, maxWidth, postWidth) =>
          preWidth `max` maxWidth `max` postWidth
      }
  }

  opaque type IndentCtx = Int
  object IndentCtx {
    def empty: IndentCtx = 0
    def fromInt(int: Int): IndentCtx = int
  }

  extension (self: IndentCtx) def value: Int = self

  inline def indentCtx(using indentCtx: IndentCtx): IndentCtx = indentCtx

  def empty: Format = Empty

  def newLine(using IndentCtx): Format =
    Newline(indentBy = indentCtx)

  def str(str: String)(using IndentCtx): Format =
    str
      .linesWithSeparators
      .map {
        case s"$line\n" =>
          Str(line)
          ++ newLine
        case line =>
          Str(line)
      }
      .reduceOption(_ `combine` _)
      .getOrElse(Empty)

  def indentedBy[T](indent: Int)(using IndentCtx)(fn: IndentCtx ?=> T): T =
    fn(using indentCtx + indent)

  given monoid: Monoid[Format] with {
    override def combine(x: Format, y: Format): Format =
      Concat(Eval.now(x), Eval.now(y))

    override def empty: Format = Empty
  }
}
