package distcompiler.parsing

import scala.annotation.targetName
import cats.*
import cats.data.*
import cats.syntax.all.given

import distcompiler.util.EvalList

sealed abstract class Parser[Elem, Input, Error: Semigroup, +T](using ops: Parser.Ops[Elem, Input, Error]) { self =>
  import Parser.*
  import ops.{*, given}

  def apply(progressIdx: Int, input: Input, leftRecCtx: LeftRecCtx[Elem, Input, Error]): Eval[R[T]]

  final def parse(input: Input): Either[Error, (T, Input)] =
    self(0, input, LeftRecCtx.empty).value match {
      case Result.Ok(value, nextInput, _, _, _) => Right((value, nextInput))
      case Result.Backtrack(error, _, _) =>
        error match {
          case Left(leftRecErr) => throw leftRecErr
          case Right(error) => Left(error)
        }
      case Result.Fatal(error, _) => Left(error)
    }    

  final def parseAll(input: Input): EvalList[Either[Error, T]] = {
    def impl(input: Input): EvalList[Either[Error, T]] =
      // parse EOF here, because that's the most reasonable way to check for EOF,
      // in which case we happily end parsing
      EvalList.defer {
        (self.left | eof.right).parse(input) match {
          case Left(error) => EvalList.single(Left(error))
          case Right((Right(()), _)) => EvalList.empty
          case Right((Left(value), nextInput)) => Right(value) +: impl(nextInput)
        }
      }

    impl(input)
  }

  final def widenK[F[_], U, S >: U](using ev: T <:< F[U])(using Functor[F]): P[F[S]] =
    self.map { v => ev(v).widen }

  final def map[U](fn: T => U): P[U] =
    Applicative[P].map(self)(fn)

  final def left: P[Left[T, Nothing]] = self.map(Left(_))

  final def right: P[Right[Nothing, T]] = self.map(Right(_))

  final def assertProgress(using enclosing: sourcecode.Enclosing): P[T] =
    P { (progressIdx, input, leftRecCtx) =>
      self(progressIdx, input, leftRecCtx).map {
        case Result.Ok(_, _, `progressIdx`, _, _) =>
          throw AssertionError(s"Lack of progress detected! This would mean something like rep() is spinning in place.\nin parser location: ${enclosing.value}")
        case otherwise => otherwise
      }
    }

  /**
    * Creates a parser that ignores progress during backtracking.
    * 
    * This means that x.peek | y will (a) always consider y, and (b) report errors from x and y on failure.
    * Without peek, x | y will ignore y entirely if x backtracks after making progress, and will consider such backtracking fatal.
    * 
    * Also, (x | y).peek will not prevent x or y from causing a fatal error by backtracking. Use it wherever non-fatal backtracking is needed.
    * x ~? y, x ~>? y, and x <~? y cover common utility cases of "let | consider backtracks coming from both x and y"
    */
  final def peek: P[T] =
    P { (progressIdx, input, leftRecCtx) =>
      self(progressIdx, input, leftRecCtx).map {
        case result @ Result.Ok(_, _, _, _, _) => result
        case result @ Result.Backtrack(_, _, _) => result.copy(progressIdx = progressIdx)
        case result @ Result.Fatal(_, _) =>
          // even with fatal error, reset progress so any relevant backtracking info gets integrated
          result.copy(progressIdx = progressIdx)
      }
    }

  final def log(name: String): P[T] =
    ops.log(name)(this)

  final def andThen[U](fn: T => P[U]): P[U] =
    P { (progressIdx, input, leftRecCtx) =>
      val origProgressIdx = progressIdx
      self(progressIdx, input, leftRecCtx).flatMap { result =>
        seqResults(origProgressIdx, leftRecCtx, result, fn, (l, r) => r, _.mapInner(_.andThen(fn)))
      }
    }

  final def constrain[U](fn: T => Either[Error, U]): P[U] =
    P { (progressIdx, input, leftRecCtx) =>
      self(progressIdx, input, leftRecCtx).map {
        case result @ Result.Ok(value, _, _, backtrackedOpt, leftRecDict) =>
          fn(value) match {
            case Left(error) => Result.Backtrack(Right(backtrackedOpt.fold(error)(_ `combine` error)), progressIdx, leftRecDict.mapInner(_.constrain(fn)))
            case Right(value) =>
              result.copy[Elem, Input, Error, U](
                value = value,
                leftRecDict = leftRecDict.mapInner(_.constrain(fn)))
          }
        case result @ Result.Backtrack(_, _, leftRecDict) =>
          result.copy[Elem, Input, Error, U](
            leftRecDict = leftRecDict.mapInner(_.constrain(fn)))
        case result @ Result.Fatal(_, _) => result.copy[Elem, Input, Error, U]()
      }
    }

  final def mapPositioned[T2, U](using ev: T <:< (T2, SourceLocation))(fn: SourceLocation ?=> T2 => U): P[U] =
    self.map { pair =>
      val (value, loc) = ev.apply(pair)
      fn(using loc)(value)
    }

  @targetName("or")
  final def |[U](other: P[U]): P[T | U] = {
    P { (progressIdx, input, leftRecCtx) =>
      val origProgressIdx = progressIdx
      self(progressIdx, input, leftRecCtx).flatMap {
        case result @ Result.Ok(_, _, _, _, leftRecDict) =>
          Eval.now(result)
        case Result.Backtrack(errorOpt, progressIdx, _) if progressIdx != origProgressIdx =>
          // lhs backtracking after progress --> give up, record one error
          errorOpt match {
            case Left(leftRecErr) => throw leftRecErr
            case Right(error) =>
              Eval.now(Result.Fatal(error, progressIdx)) // note: throw away leftRecDict because fatal
          }
        case Result.Backtrack(errorOpt, _, leftRecDict) =>
          val outerLeftRecDict = leftRecDict
          // lhs backtracking with no progress --> try rhs and potentially carry over error info
          other(origProgressIdx, input, leftRecCtx).map {
            case result @ Result.Ok(_, _, progressIdx, _, leftRecDict) if progressIdx != origProgressIdx =>
              // rhs succeeds after progress --> we moved on from whatever we backtracked, drop old error info
              result.copy[Elem, Input, Error, T | U](
                leftRecDict = outerLeftRecDict.combine(leftRecDict))
            case result @ Result.Ok(_, _, _, backtrackedOpt, leftRecDict) =>
              // rhs succeeds with no progress --> good, but keep old error for display because we haven't moved yet
              result.copy(
                backtrackedOpt = errorOpt.toOption `combineOptions` backtrackedOpt,
                leftRecDict = outerLeftRecDict.combine(leftRecDict))
            case Result.Backtrack(errorOpt2, progressIdx, _) if progressIdx != origProgressIdx =>
              // rhs backtracking after progress --> give up (but record both errors)
              Result.Fatal((errorOpt.toOption `combineOptions` errorOpt2.toOption).get, progressIdx)
            case Result.Backtrack(errorOpt2, progressIdx, leftRecDict) =>
              // backtrack with no progress --> try again (recording both errors)
              Result.Backtrack(errorOpt `combineEithers` errorOpt2, progressIdx, outerLeftRecDict.combine(leftRecDict))
            case result @ Result.Fatal(_, progressIdx) if progressIdx != origProgressIdx =>
              // rhs fatal after progress --> forward with no changes, our backtracking info is irrelevant
              result
            case Result.Fatal(error2, progressIdx) =>
              // rhs fatal with no progress --> still same position, add our error info
              Result.Fatal(errorOpt.toOption.fold(error2)(_ `combine` error2), progressIdx)
          }
        case result @ Result.Fatal(_, _) => Eval.now(result)
      }
    }
  }

  @targetName("ignoreSeq")
  final def ~>[U](other: P[U]): P[U] =
    self.productR(other)

  @targetName("seqIgnore")
  final def <~(other: P[?]): P[T] =
    self.productL(other)

  @targetName("ignoreSeqPeek")
  final def ~>?[U](other: P[U]): P[U] =
    self.productR(other).peek

  @targetName("seqIgnorePeek")
  final def <~?(other: P[?]): P[T] =
    self.productL(other).peek

  @targetName("seq")
  final def ~[U](other: P[U]): P[T ~ U] =
    self.product(other).map { case (l, r) => l ~ r }

  @targetName("seqPeek")
  final def ~?[U](other: P[U]): P[T ~ U] =
    self.product(other)
      .map { case (l, r) => l ~ r }
      .peek
}

object Parser {
  final case class LeftRecError() extends Exception

  extension [T: Semigroup](self: Option[T]) private[Parser] def combineOptions(other: Option[T]): Option[T] =
    (self, other) match {
      case (None, other) => other
      case (self, None) => self
      case (Some(self), Some(other)) => Some(self `combine` other)
    }

  extension [T: Semigroup](self: Either[LeftRecError, T]) private[Parser] def combineEithers(other: Either[LeftRecError, T]): Either[LeftRecError, T] =
    (self, other) match {
      case (self @ Left(_), Left(_)) => self // left bias might be more usable?
      case (self, Left(_)) => self
      case (Left(_), other) => other
      case (Right(self), Right(other)) => Right(self `combine` other)
    }

  final infix case class ~[+A, +B](a: A, b: B)
  extension [A](a: A) {
    @targetName("andThen")
    infix def ~[B](b: B): A ~ B = new ~(a, b)
  }

  final class Ops[Elem, Input, Error: Semigroup](using inputOps: InputOps[Elem, Input, Error])(using errorOps: ErrorOps[Elem, Error]) {
    given ops: Ops[Elem, Input, Error] = this // need this or we'll allocate too many new ops, including recursively

    type P[T] = Parser[Elem, Input, Error, T]
    object P {
      private[Parser] def apply[T](fn: (Int, Input, LeftRecCtx[Elem, Input, Error]) => Eval[R[T]]): P[T] = new Parser[Elem, Input, Error, T] {
        override def apply(progressIdx: Int, input: Input, leftRecCtx: LeftRecCtx[Elem, Input, Error]): Eval[R[T]] =
          /*Eval.defer(*/fn(progressIdx, input, leftRecCtx)//)
      }
    }
    type R[T] = Result[Elem, Input, Error, T]

    export Parser.~

    given nonEmptyAlternative: NonEmptyAlternative[P] with {
      override def pure[A](x: A): P[A] = trivial(x)
      override def ap[A, B](ff: P[A => B])(fa: P[A]): P[B] =
        P { (progressIdx, input, leftRecCtx) =>
          val origProgressIdx = progressIdx
          ff(progressIdx, input, leftRecCtx).flatMap { result =>
            seqResults(origProgressIdx, leftRecCtx, result, _ => fa, _(_), _.mapInner(_.ap(fa)))
          }
        }

      override def combineK[A](x: P[A], y: P[A]): P[A] = x | y
    }

    def peek[T](parser: P[T]): P[T] =
      parser.peek

    def trivial[T](value: T): P[T] =
      P { (progressIdx, input, leftRecCtx) =>
        Eval.now(Result.Ok(value, input, progressIdx, None, LeftRecDict.empty))
      }

    val eof: P[Unit] =
      P { (progressIdx, input, leftRecCtx) =>
        inputOps.read(input) match {
          case Left(err) => Eval.now(Result.Fatal(err, progressIdx))
          case Right(None) => Eval.now(Result.Ok((), input, progressIdx, None, LeftRecDict.empty))
          case Right(Some((actualElem, nextInput))) =>
            Eval.now(Result.Backtrack(
              Right(errorOps.expectedEOF(
                sourceLocation = inputOps.lastSourceLocation(nextInput),
                actualElem = actualElem)),
              progressIdx,
              LeftRecDict.empty))
        }
      }

    val input: P[Input] =
      P { (progressIdx, input, leftRecCtx) =>
        Eval.now(Result.Ok(input, input, progressIdx, None, LeftRecDict.empty))
      }

    val anyElem: P[Elem] =
      P { (progressIdx, input, leftRecCtx) =>
        inputOps.read(input) match {
          case Left(err) =>
            Eval.now(Result.Fatal(err, progressIdx))
          case Right(None) =>
            Eval.now(Result.Backtrack(Right(errorOps.unexpectedEOF(inputOps.nextSourceLocation(input))), progressIdx, LeftRecDict.empty))
          case Right(Some((elem, nextInput))) =>
            Eval.now(Result.Ok(elem, nextInput, progressIdx + 1, None, LeftRecDict.empty))
        }
      }

    def log[T](name: String)(parser: P[T]): P[T] =
      P { (progressIdx, input, leftRecCtx) =>
        val origProgressIdx = progressIdx
        println(s"[$name@$progressIdx] entering")
        parser(progressIdx, input, leftRecCtx).map {
          case res @ Result.Ok(value, nextInput, progressIdx, backtrackedOpt, _) =>
            println(s"[$name@$origProgressIdx] parsed $value, progressIdx=$progressIdx and backtrackedOpt=$backtrackedOpt")
            res
          case res @ Result.Backtrack(error, progressIdx, _) =>
            println(s"[$name@$origProgressIdx] backtrack $error, progressIdx=$progressIdx")
            res
          case res @ Result.Fatal(error, progressIdx) =>
            println(s"[$name@$origProgressIdx] fatal $error, progressIdx=$progressIdx")
            res
        }
      }

    def backtrack[T](error: Error): P[T] =
      P { (progressIdx, _, _) => Eval.now(Result.Backtrack(Right(error), progressIdx, LeftRecDict.empty)) }

    def phrase[T](body: P[T]): P[T] =
      body <~ eof

    def opt[T](elem: P[T]): P[Option[T]] =
      elem.map(Some(_)) | trivial(None)

    def lzy[T](parser: =>P[T]): P[T] = {
      lazy val parserLzy = parser
      P { (progressIdx, input, leftRecCtx) =>
        parserLzy(progressIdx, input, leftRecCtx)
      }
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

    def capturingPosition[T](parser: P[T]): P[(T, SourceLocation)] =
      (input.map(inputOps.nextSourceLocation) ~ parser ~ input.map(inputOps.nextSourceLocation)).map {
        case prePos ~ value ~ postPos => (value, prePos.combine(postPos))
      }

    def leftRec[T](using sourcecode.Enclosing)(parser: P[T]): P[T] = {
      val leftRecError = LeftRecError()
      lazy val self: P[T] =
        P { (progressIdx, input, leftRecCtx) =>
          val inputIdx = inputOps.tellIdx(input)
          val anchor = (inputIdx, self)
          if(leftRecCtx.contains(anchor)) {
            Eval.now(Result.Backtrack(Left(leftRecError), progressIdx, LeftRecDict.single(anchor)(trivial)))
          } else {
            parser.assertProgress(progressIdx, input, leftRecCtx.incl(anchor)).flatMap {
              case result @ Result.Ok(value, nextInput, progressIdx, backtrackedOpt, leftRecDict) =>
                leftRecDict.get(anchor) match {
                  case None =>
                    Eval.now(result)
                  case Some(growFn) =>
                    def growImpl(smallerValue: T): P[T] =
                      growFn(smallerValue).value.assertProgress.andThen(growImpl)
                      | trivial(smallerValue)

                    growImpl(value)(progressIdx, nextInput, leftRecCtx)
                }
              case result @ (Result.Backtrack(_, _, _) | Result.Fatal(_, _)) =>
                Eval.now(result)
            }
          }
        }
      self
    }

    private[parsing] def seqResults[T, U, V](origProgressIdx: Int, leftRecCtx: LeftRecCtx[Elem, Input, Error], left: R[T], rightP: T => P[U], combine: (T, U) => V, rightDict: LeftRecDict[Elem, Input, Error, T] => LeftRecDict[Elem, Input, Error, V]): Eval[R[V]] = {
      left match {
        case Result.Ok(leftValue, nextInput, progressIdx, backtrackedOpt, leftRecDict) =>
          assert(origProgressIdx <= progressIdx, "progressIdx must be monotonic")
          val firstProgressIdx = progressIdx
          // keep first backtrackedOpt only if we have not made progress.
          // If we moved, we're done peeking or choosing.
          val firstBacktrackedOpt =
            if(progressIdx != origProgressIdx) {
              None
            } else {
              backtrackedOpt
            }
          def adaptOnlyLeftRecDict2(leftRecDict2: LeftRecDict[Elem, Input, Error, U]): LeftRecDict[Elem, Input, Error, V] =
            leftRecDict2.mapInner(_.map(combine(leftValue, _)))
          
          def adaptLeftRecDict(leftRecDict2: LeftRecDict[Elem, Input, Error, U]): LeftRecDict[Elem, Input, Error, V] =
            rightDict(leftRecDict) ++ adaptOnlyLeftRecDict2(leftRecDict2)

          rightP(leftValue)(progressIdx, nextInput, leftRecCtx).map {
            case result @ Result.Ok(rightValue, _, _, _, leftRecDict) if progressIdx != firstProgressIdx =>
              // second op succeeded after progress --> keep only second op info
              result.copy[Elem, Input, Error, V](
                value = combine(leftValue, rightValue),
                leftRecDict = adaptOnlyLeftRecDict2(leftRecDict))
            case result @ Result.Ok(rightValue, _, _, backtrackedOpt, leftRecDict) =>
              // second op succeeded with no progress --> merge first op info (if we kept it) and second op info
              result.copy[Elem, Input, Error, V](
                value = combine(leftValue, rightValue),
                backtrackedOpt = firstBacktrackedOpt `combineOptions` backtrackedOpt,
                leftRecDict = adaptLeftRecDict(leftRecDict))
            case result @ Result.Backtrack(_, _, leftRecDict) if progressIdx != firstProgressIdx =>
              // second op backtracked after progress --> keep only second op backtrack info
              result.copy[Elem, Input, Error, V](
                leftRecDict = adaptOnlyLeftRecDict2(leftRecDict))
            case result @ Result.Backtrack(error, _, leftRecDict) =>
              // second op backtracked with no progress --> merge first op info (if we kept it) and second op info
              result.copy[Elem, Input, Error, V](
                errorOpt = firstBacktrackedOpt.fold(error)(firstError => Right(firstError) `combineEithers` error),
                leftRecDict = adaptLeftRecDict(leftRecDict))
            case result @ Result.Fatal(_, progressIdx) if progressIdx != firstProgressIdx =>
              // second op fatal with progress --> not our problem, forward
              result.copy[Elem, Input, Error, V]()
            case result @ Result.Fatal(error, _) =>
              // second op fatal with no progress --> merge our error info, still same place
              result.copy[Elem, Input, Error, V](
                error = firstBacktrackedOpt.fold(error)(_ `combine` error))
          }
        case result @ Result.Backtrack(_, _, leftRecDict) =>
          Eval.now(result.copy[Elem, Input, Error, V](
            leftRecDict = rightDict(leftRecDict)))
        case result @ Result.Fatal(_, _) =>
          Eval.now(result.copy[Elem, Input, Error, V]())
      }
    }
  }
  
  object Ops {
    def apply[Elem, Input, Error: Semigroup](using inputOps: InputOps[Elem, Input, Error])(using errorOps: ErrorOps[Elem, Error]): Ops[Elem, Input, Error] =
      new Ops
  }

  enum Result[Elem, Input, Error, +T] {
    case Ok(value: T, nextInput: Input, progressIdx: Int, backtrackedOpt: Option[Error], leftRecDict: LeftRecDict[Elem, Input, Error, T])
    case Backtrack(errorOpt: Either[LeftRecError, Error], progressIdx: Int, leftRecDict: LeftRecDict[Elem, Input, Error, T])
    case Fatal(error: Error, progressIdx: Int)

    def map[U](fn: T => U): Result[Elem, Input, Error, U] =
      this match {
        case result @ Ok(value, _, _, _, leftRecDict) => 
          result.copy[Elem, Input, Error, U](value = fn(value), leftRecDict = leftRecDict.mapInner(_.map(fn)))
        case result @ Backtrack(_, _, leftRecDict) =>
          result.copy[Elem, Input, Error, U](leftRecDict = leftRecDict.mapInner(_.map(fn)))
        case result @ Fatal(_, _) =>
          result.copy[Elem, Input, Error, U]()
      }
  }

  object LeftRecCtxImpl {
    opaque type LeftRecCtx[Elem, Input, Error] = Set[(Int, Parser[Elem, Input, Error, ?])]

    extension [Elem, Input, Error](self: LeftRecCtx[Elem, Input, Error]) {
      def contains(key: (Int, Parser[Elem, Input, Error, ?])): Boolean =
        self.contains(key)

      def incl(key: (Int, Parser[Elem, Input, Error, ?])): LeftRecCtx[Elem, Input, Error] =
        self.incl(key)
    }

    object LeftRecCtx {
      def empty[Elem, Input, Error]: LeftRecCtx[Elem, Input, Error] = Set.empty
    }
  }

  export LeftRecCtxImpl.LeftRecCtx

  object LeftRecDictImpl {
    opaque type LeftRecDict[Elem, Input, Error, +T] = Map[(Int, Parser[Elem, Input, Error, ?]), Any => Eval[Parser[Elem, Input, Error, T]]]

    extension [Elem, Input, Error, T](self: LeftRecDict[Elem, Input, Error, T]) {
      def nonEmpty: Boolean = self.nonEmpty

      private[Parser] def mapInner[U](fn: Parser[Elem, Input, Error, T] => Parser[Elem, Input, Error, U]): LeftRecDict[Elem, Input, Error, U] =
        self
          .view
          .mapValues(innerFn => (from: Any) => Eval.defer(innerFn(from)).map(fn))
          .toMap

      def contains(key: (Int, Parser[Elem, Input, Error, T])): Boolean =
        self.contains(key)

      def get(key: (Int, Parser[Elem, Input, Error, T])): Option[T => Eval[Parser[Elem, Input, Error, T]]] =
        self.get(key)

      def ++(other: LeftRecDict[Elem, Input, Error, T]): LeftRecDict[Elem, Input, Error, T] =
        self ++ other

      def combine(other: LeftRecDict[Elem, Input, Error, T]): LeftRecDict[Elem, Input, Error, T] =
        (self.keysIterator ++ other.keysIterator)
          .map { key =>
            (self.get(key), other.get(key)) match {
              case (None, other) => other.map(key -> _)
              case (self, None) => self.map(key -> _)
              case (Some(selfFn), Some(otherFn)) => Some((key, ((from: Any) => (selfFn(from), otherFn(from)).mapN(_ | _))))
            }
          }
          .flatten
          .toMap
    }

    object LeftRecDict {
      def empty[Elem, Input, Error, T]: LeftRecDict[Elem, Input, Error, T] = Map.empty

      def single[Elem, Input, Error, T](anchor: (Int, Parser[Elem, Input, Error, T]))(fn: T => Parser[Elem, Input, Error, T]): LeftRecDict[Elem, Input, Error, T] =
        Map(anchor -> (t => Eval.now(fn(t.asInstanceOf[T]))))
    }
  }

  export LeftRecDictImpl.LeftRecDict
}
