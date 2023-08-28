package distcompiler.parsing

import cats.*
import cats.data.*
import cats.syntax.all.given

import scala.annotation.targetName
import scala.collection.mutable

import distcompiler.util.EvalList

enum Parser[Elem, Input, Error, +T] {
  private type P[+T] = Parser[Elem, Input, Error, T]

  case ReadElem() extends Parser[Elem, Input, Error, Elem]
  case AtEof() extends Parser[Elem, Input, Error, Unit]
  case Input() extends Parser[Elem, Input, Error, Input]
  case Trivial(value: T)
  case Peek(p: Parser[Elem, Input, Error, T])
  case Lazy(handle: Parser.Handle, pFn: () => Parser[Elem, Input, Error, T])
  case Constrain[Elem, Input, Error, T, U](p: Parser[Elem, Input, Error, T], pred: T => Either[Error, U]) extends Parser[Elem, Input, Error, U]
  case Or[Elem, Input, Error, T, U](left: Parser[Elem, Input, Error, T], right: Parser[Elem, Input, Error, U]) extends Parser[Elem, Input, Error, T | U]
  case Ap[Elem, Input, Error, T, U](ff: Parser[Elem, Input, Error, T => U], fa: Parser[Elem, Input, Error, T]) extends Parser[Elem, Input, Error, U]
  case AndThen[Elem, Input, Error, T, U](pt: Parser[Elem, Input, Error, T], fn: T => Parser[Elem, Input, Error, U]) extends Parser[Elem, Input, Error, U]

  val mustMakeProgress: Boolean =
    this match {
      case ReadElem() => true
      case AtEof() => false
      case Input() => false
      case Trivial(_) => false
      case Peek(p) => p.mustMakeProgress
      case Lazy(_, _) => false
      case Constrain(p, _) => p.mustMakeProgress
      case Or(left, right) => left.mustMakeProgress && right.mustMakeProgress
      case Ap(ff, fa) => ff.mustMakeProgress || fa.mustMakeProgress
      case AndThen(pt, _) => pt.mustMakeProgress
    }

  def assertProgress: P[T] = {
    assert(mustMakeProgress, "lack of progress detected! (could not prove that the parser won't get stuck)")
    this
  }

  def parse(input: Input)(using inputOps: InputOps[Elem, Input, Error], errorOps: ErrorOps[Elem, Error]): Either[NonEmptyList[Error], (T, Input)] = {
    enum Result[+T] derives CanEqual {
      case Ok(value: T, nextInput: Input, recoveredErrors: Chain[Error])
      case Backtrack(errors: Chain[Error], idx: Int) extends Result[Nothing]
      case Fatal(errors: Chain[Error], idx: Int) extends Result[Nothing]
    }

    extension (input: Input) def idx: Int = inputOps.tellIdx(input)

    type Anchor = (Parser.Handle, Int)
    val leftRecTable = mutable.HashMap[Anchor, Option[Any]]()
    val leftRecReached = mutable.HashSet[Anchor]()

    def impl[T](self: P[T], input: Input): Eval[Result[T]] =
      self match {
        case ReadElem() =>
          inputOps.read(input) match {
            case Left(error) =>
              Eval.now(Result.Fatal(Chain.one(error), input.idx))
            case Right(Some(elem, nextInput)) =>
              Eval.now(Result.Ok(elem, nextInput, Chain.nil)) 
            case Right(None) =>
              Eval.now(Result.Backtrack(
                Chain.one(errorOps.unexpectedEOF(inputOps.nextSourceLocation(input))),
                input.idx))
          }
        case AtEof() =>
          inputOps.read(input) match {
            case Left(error) =>
              Eval.now(Result.Fatal(Chain.one(error), input.idx))
            case Right(Some((elem, nextInput))) =>
              Eval.now(Result.Backtrack(
                Chain.one(errorOps.expectedEOF(inputOps.lastSourceLocation(nextInput), elem)),
                input.idx))
            case Right(None) =>
              Eval.now(Result.Ok((), input, Chain.nil))
          }
        case Input() => 
          Eval.now(Result.Ok(input, input, Chain.nil))
        case Trivial(value) =>
          Eval.now(Result.Ok(value, input, Chain.nil))
        case Peek(p) =>
          impl(p, input).map {
            case result @ Result.Ok(_, _, _) => result
            case Result.Backtrack(errors, _) => Result.Backtrack(errors, input.idx)
            case result @ Result.Fatal(_, _) => result
          }
        case Lazy(handle, pFn) =>
          val anchor = (handle, inputOps.tellIdx(input))
          leftRecTable.get(anchor) match {
            case None =>
              leftRecTable.update(anchor, None)
              def grow(result: Result[T], smallerValue: Option[T], prevIdx: Int): Eval[Result[T]] =
                result match {
                  case result @ Result.Ok(value, nextInput, _) =>
                    if(leftRecReached(anchor)) {
                      leftRecReached.remove(anchor)
                      leftRecTable.update(anchor, Some(value))

                      assert(prevIdx < nextInput.idx)
                      impl(pFn(), nextInput)
                        .flatMap(grow(_, Some(value), nextInput.idx))
                    } else {
                      Eval.now(result)
                    }
                  case Result.Backtrack(errors, _) =>
                    smallerValue match {
                      case None =>
                        Eval.now(result)
                      case Some(value) =>
                        Eval.now(Result.Ok(value, input, recoveredErrors = errors))
                    }
                  case result @ Result.Fatal(_, _) => Eval.now(result)
                }

              impl(pFn(), input)
                .flatMap(grow(_, None, input.idx))
                .productL(Eval.always { 
                  leftRecReached.remove(anchor)
                  leftRecTable.remove(anchor)
                })
            case Some(None) =>
              leftRecReached.add(anchor)
              Eval.now(Result.Backtrack(Chain.nil, input.idx))
            case Some(Some(value)) =>
              leftRecReached.add(anchor)
              Eval.now(Result.Ok(value.asInstanceOf[T], input, Chain.nil))
          }
        case Constrain(p, pred) =>
          impl(p, input).map {
            case Result.Ok(value, nextInput, recoveredErrors) =>
              pred(value) match {
                case Left(error) =>
                  // if we're constraining a length 1 parse, special-case that we backtrack with no progress.
                  // otherwise, let someone else add peek if they want
                  val nextIdx =
                    if(nextInput.idx == input.idx + 1) {
                      input.idx
                    } else {
                      nextInput.idx
                    }
                  Result.Backtrack(recoveredErrors :+ error, nextIdx)
                case Right(value) =>
                  Result.Ok(value, nextInput, recoveredErrors)
              }
            case result @ (Result.Backtrack(_, _) | Result.Fatal(_, _)) => result
          }
        case Or(left, right) =>
          impl(left, input).flatMap {
            case result @ Result.Ok(_, _, _) =>
              Eval.now(result)
            case Result.Backtrack(errors, idx) if idx == input.idx =>
              val outerIdx = idx
              val outerErrors = errors
              impl(right, input).map {
                case result @ Result.Ok(value, nextInput, recoveredErrors) if nextInput.idx == idx => 
                  result.copy(recoveredErrors = outerErrors ++ recoveredErrors)
                case result @ Result.Backtrack(errors, `outerIdx`) =>
                  result.copy(errors = outerErrors ++ errors)
                case result @ Result.Fatal(errors, `outerIdx`) =>
                  result.copy(errors = outerErrors ++ errors)
                case result => result
              }
            case Result.Backtrack(errors, idx) =>
              Eval.now(Result.Fatal(errors, idx))
            case result @ Result.Fatal(_, _) =>
              Eval.now(result)
          }
        case Ap(ff, fa) =>
          impl(ff, input).flatMap {
            case Result.Ok(ff, nextInput, recoveredErrors) =>
              val outerIdx = nextInput.idx
              val outerErrors = recoveredErrors
              impl(fa, nextInput).map {
                case Result.Ok(fa, nextInput, recoveredErrors) if nextInput.idx == outerIdx =>
                  Result.Ok(ff(fa), nextInput, outerErrors ++ recoveredErrors)
                case result @ Result.Ok(fa, _, _) =>
                  result.copy(value = ff(fa))
                case result @ Result.Backtrack(errors, `outerIdx`) =>
                  result.copy(errors = outerErrors ++ errors)
                case result @ Result.Fatal(errors, `outerIdx`) =>
                  result.copy(errors = outerErrors ++ errors)
                case result @ (Result.Backtrack(_, _) | Result.Fatal(_, _)) =>
                  result
              }
            case result @ (Result.Backtrack(_, _) | Result.Fatal(_, _)) =>
              Eval.now(result)
          }
        case AndThen(pt, fn) =>
          impl(pt, input).flatMap {
            case Result.Ok(t, nextInput, recoveredErrors) =>
              val outerIdx = nextInput.idx
              val outerErrors = recoveredErrors
              impl(fn(t), nextInput).map {
                case result @ Result.Ok(fa, nextInput, recoveredErrors) if nextInput.idx == outerIdx =>
                  result.copy(recoveredErrors = outerErrors ++ recoveredErrors)
                case result @ Result.Backtrack(errors, `outerIdx`) =>
                  result.copy(errors = outerErrors ++ errors)
                case result @ Result.Fatal(errors, `outerIdx`) =>
                  result.copy(errors = outerErrors ++ errors)
                case result @ (Result.Ok(_, _, _) | Result.Backtrack(_, _) | Result.Fatal(_, _)) =>
                  result
              }
            case result @ (Result.Backtrack(_, _) | Result.Fatal(_, _)) =>
              Eval.now(result)
          }
      }
      
    impl(this, input).value match {
      case Result.Ok(value, nextInput, _) =>
        Right((value, nextInput))
      case Result.Backtrack(errors, _) =>
        Left(NonEmptyList.fromListUnsafe(errors.toList))
      case Result.Fatal(errors, _) =>
        Left(NonEmptyList.fromListUnsafe(errors.toList))
    }
  }

  def parseAll(input: Input)(using inputOps: InputOps[Elem, Input, Error], errorOps: ErrorOps[Elem, Error], ops: Parser.Ops[Elem, Input, Error]): EvalList[Either[NonEmptyList[Error], T]] = {
    import ops.{input as _, *}
    def impl(input: Input): EvalList[Either[NonEmptyList[Error], T]] =
      // parse EOF here, because that's the most reasonable way to check for EOF,
      // in which case we happily end parsing
      EvalList.defer {
        (this.toLeft | eof.toRight).parse(input) match {
          case Left(errors) => EvalList.single(Left(errors))
          case Right((Right(()), _)) => EvalList.empty
          case Right((Left(value), nextInput)) => Right(value) +: impl(nextInput)
        }
      }

    impl(input)
  }

  def toLeft: P[Left[T, Nothing]] = this.map(Left(_))

  def toRight: P[Right[Nothing, T]] = this.map(Right(_))

  def map[U](fn: T => U): P[U] =
    Ap(Trivial(fn), this)

  def mapPositioned[U, V](using ev: T <:< (U, SourceLocation))(fn: SourceLocation ?=> U => V): P[V] =
    this.map(pair => fn(using ev(pair)._2)(ev(pair)._1))

  /**
    * Creates a parser that ignores progress during backtracking.
    * 
    * This means that x.peek | y will (a) always consider y, and (b) report errors from x and y on failure.
    * Without peek, x | y will ignore y entirely if x backtracks after making progress, and will consider such backtracking fatal.
    * 
    * Also, (x | y).peek will not prevent x or y from causing a fatal error by backtracking. Use it wherever non-fatal backtracking is needed.
    * x ~? y, x ~>? y, and x <~? y cover common utility cases of "let | consider backtracks coming from both x and y"
    */
  def peek: P[T] =
    Peek(this)

  // // def log(name: String): P[T] =
  // //   ops.log(name)(this)

  def andThen[U](fn: T => P[U]): P[U] =
    AndThen(this, fn)

  def constrain[U](pred: T => Either[Error, U]): P[U] =
    Constrain(this, pred)

  @targetName("or")
  def |[U](other: P[U]): P[T | U] =
    Or(this, other)
}

object Parser {
  final class Handle

  final infix case class ~[+A, +B](a: A, b: B)
  extension [A](a: A) {
    @targetName("seq")
    infix def ~[B](b: B): A ~ B = new ~(a, b)
  }

  extension [Elem, Input, Error, T](using ops: Ops[Elem, Input, Error])(self: ops.P[T]) {
    @targetName("ignoreSeq")
    def ~>[U](other: ops.P[U]): ops.P[U] =
      ops.nonEmptyAlternative.productR(self)(other)

    @targetName("seqIgnore")
    def <~(other: ops.P[?]): ops.P[T] =
      ops.nonEmptyAlternative.productL(self)(other)

    @targetName("ignoreSeqPeek")
    def ~>?[U](other: ops.P[U]): ops.P[U] =
      (self ~> other).peek

    @targetName("seqIgnorePeek")
    def <~?(other: ops.P[?]): ops.P[T] =
      (self <~ other).peek

    @targetName("seq")
    def ~[U](other: ops.P[U]): ops.P[T ~ U] =
      ops.nonEmptyAlternative.product(self, other).map {
        case (l, r) => l ~ r
      }

    @targetName("seqPeek")
    def ~?[U](other: ops.P[U]): ops.P[T ~ U] =
      (self ~ other).peek
  }

  final class Ops[Elem, Input, Error] {
    given ops: Ops[Elem, Input, Error] = this // need this or we'll allocate too many new ops, including recursively

    type P[T] = Parser[Elem, Input, Error, T]

    export Parser.~

    given nonEmptyAlternative: NonEmptyAlternative[P] with {
      override def pure[A](x: A): P[A] = trivial(x)
      override def ap[A, B](ff: P[A => B])(fa: P[A]): P[B] =
        Parser.Ap(ff, fa)
      override def combineK[A](x: P[A], y: P[A]): P[A] = x | y
    }

    def peek[T](parser: P[T]): P[T] =
      parser.peek

    def trivial[T](value: T): P[T] = Parser.Trivial(value)

    val eof: P[Unit] = Parser.AtEof()

    val input: P[Input] = Parser.Input()

    val anyElem: P[Elem] = Parser.ReadElem()

    // def log[T](name: String)(parser: P[T]): P[T] =
    //   P { (progressIdx, input, leftRecCtx) =>
    //     val origProgressIdx = progressIdx
    //     println(s"[$name@$progressIdx] entering")
    //     parser(progressIdx, input, leftRecCtx).map {
    //       case res @ Result.Ok(value, nextInput, progressIdx, backtrackedOpt, _) =>
    //         println(s"[$name@$origProgressIdx] parsed $value, progressIdx=$progressIdx and backtrackedOpt=$backtrackedOpt")
    //         res
    //       case res @ Result.Backtrack(error, progressIdx, _) =>
    //         println(s"[$name@$origProgressIdx] backtrack $error, progressIdx=$progressIdx")
    //         res
    //       case res @ Result.Fatal(error, progressIdx) =>
    //         println(s"[$name@$origProgressIdx] fatal $error, progressIdx=$progressIdx")
    //         res
    //     }
    //   }

    def phrase[T](body: P[T]): P[T] =
      body <~ eof

    def opt[T](elem: P[T]): P[Option[T]] =
      elem.map(Some(_)) | trivial(None)

    def lzy[T](parser: =>P[T]): P[T] = {
      val handle = new Handle
      lazy val parserLzy = parser
      Parser.Lazy(handle, () => parserLzy)
    }

    def rep[T](elem: P[T])(using sourcecode.Enclosing): P[Chain[T]] =
      (elem.assertProgress, lzy(rep(elem))).mapN(_ +: _)
      | trivial(Chain.empty)

    def rep1[T](elem: P[T])(using sourcecode.Enclosing): P[NonEmptyChain[T]] =
      (elem, rep(elem)).mapN(NonEmptyChain.fromChainPrepend)

    def repsep[T](elem: P[T], sep: P[?])(using sourcecode.Enclosing): P[Chain[T]] =
      opt((elem, rep(sep ~>? elem)).mapN(_ +: _)).map(_.getOrElse(Chain.nil))

    def rep1sep[T](elem: P[T], sep: P[?])(using sourcecode.Enclosing): P[NonEmptyChain[T]] =
      (elem, rep(sep ~>? elem)).mapN(NonEmptyChain.fromChainPrepend)

    def capturingPosition[T](parser: P[T])(using inputOps: InputOps[Elem, Input, Error]): P[(T, SourceLocation)] =
      (input.map(inputOps.nextSourceLocation) ~ parser ~ input.map(inputOps.nextSourceLocation)).map {
        case prePos ~ value ~ postPos => (value, prePos.combine(postPos))
      }
  }
  
  object Ops {
    def apply[Elem, Input, Error]: Ops[Elem, Input, Error] =
      new Ops
  }
}
