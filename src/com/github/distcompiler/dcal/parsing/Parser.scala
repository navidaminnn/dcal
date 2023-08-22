package com.github.distcompiler.dcal.parsing

import scala.annotation.targetName
import cats.*
import cats.data.Chain
import cats.syntax.all.given

import com.github.distcompiler.dcal.util.EvalList
import cats.StackSafeMonad

abstract class Parser[Elem, Input, Error: Semigroup, +T](using ops: Parser.Ops[Elem, Input, Error]) { self =>
  import Parser.*
  import ops.*

  def apply(progressIdx: Int, input: Input): Eval[R[T]]

  final def parse(input: Input): Either[Error, (T, Input)] =
    self(0, input).value match {
      case Result.Ok(value, nextInput, _, _) => Right((value, nextInput))
      case Result.Backtrack(error, _) => Left(error)
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
    self.flatMap(value => trivial(fn(value)))

  final def left: P[Left[T, Nothing]] = self.map(Left(_))

  final def right: P[Right[Nothing, T]] = self.map(Right(_))

  final def assertProgress(using enclosing: sourcecode.Enclosing): P[T] =
    P { (progressIdx, input) =>
      self(progressIdx, input).map {
        case Result.Ok(_, _, `progressIdx`, _) =>
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
  final lazy val peek: P[T] =
    P { (progressIdx, input) =>
      self(progressIdx, input).map {
        case Result.Ok(value, nextInput, progressIdx, backtrackedOpt) =>
          Result.Ok(value, nextInput, progressIdx, backtrackedOpt)
        case Result.Backtrack(error, _) =>
          Result.Backtrack(error, progressIdx)
        case Result.Fatal(error, _) =>
          // even with fatal error, reset progress so any relevant backtracking info gets integrated
          Result.Fatal(error, progressIdx)
      }
    }

  final def flatMap[U](fn: T => P[U]): P[U] =
    P { (progressIdx, input) =>
      val origProgressIdx = progressIdx
      self(progressIdx, input).flatMap {
        case Result.Ok(value, nextInput, progressIdx, backtrackedOpt) =>
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

          fn(value)(progressIdx, nextInput).map {
            case result @ Result.Ok(_, _, progressIdx, _) if progressIdx != firstProgressIdx =>
              // second op succeeded after progress --> keep only second op info
              result
            case Result.Ok(value, nextInput, progressIdx, backtrackedOpt) =>
              // second op succeeded with no progress --> merge first op info (if we kept it) and second op info
              Result.Ok(value, nextInput, progressIdx, firstBacktrackedOpt
                .flatMap(err1 => backtrackedOpt.map(err1 `combine` _).orElse(Some(err1)))
                .orElse(backtrackedOpt))
            case result @ Result.Backtrack(_, progressIdx) if progressIdx != firstProgressIdx =>
              // second op backtracked after progress --> keep only second op backtrack info
              result
            case Result.Backtrack(error, progressIdx) =>
              // second op backtracked with no progress --> merge first op info (if we kept it) and second op info
              Result.Backtrack(firstBacktrackedOpt.map(_ `combine` error).getOrElse(error), progressIdx)
            case result @ Result.Fatal(_, progressIdx) if progressIdx != firstProgressIdx =>
              // second op fatal with progress --> not our problem, forward
              result
            case Result.Fatal(error, progressIdx) =>
              // second op fatal with no progress --> merge our error info, still same place
              Result.Fatal(firstBacktrackedOpt.map(_ `combine` error).getOrElse(error), progressIdx)
          }
        case Result.Backtrack(error, progressIdx) => Eval.now(Result.Backtrack(error, progressIdx))
        case Result.Fatal(error, progressIdx) => Eval.now(Result.Fatal(error, progressIdx))
      }
    }

  final def constrain[U](fn: T => Either[Error, U]): P[U] =
    self
      .flatMap { value =>
        fn(value) match {
          case Left(error) => backtrack(error)
          case Right(value) => trivial(value)
        }
      }
      .peek

  final def mapPositioned[T2, U](using ev: T <:< (T2, SourceLocation))(fn: SourceLocation ?=> T2 => U): P[U] =
    self.map { pair =>
      val (value, loc) = ev.apply(pair)
      fn(using loc)(value)
    }

  @targetName("or")
  final def |[U](other: =>P[U]): P[T | U] = {
    lazy val otherLazy = other
    P { (progressIdx, input) =>
      val origProgressIdx = progressIdx
      self(progressIdx, input).flatMap {
        case Result.Ok(value, nextInput, progressIdx, backtrackedOpt) =>
          Eval.now(Result.Ok(value, nextInput, progressIdx, backtrackedOpt))
        case Result.Backtrack(error, progressIdx) if progressIdx != origProgressIdx =>
          // lhs backtracking after progress --> give up, record one error
          Eval.now(Result.Fatal(error, progressIdx))
        case Result.Backtrack(error, _) =>
          // lhs backtracking with no progress --> try rhs and potentially carry over error info
          otherLazy(origProgressIdx, input).map {
            case Result.Ok(value, nextInput, progressIdx, _) if progressIdx != origProgressIdx =>
              // rhs succeeds after progress --> we moved on from whatever we backtracked, drop old error info
              Result.Ok(value, nextInput, progressIdx, None)
            case Result.Ok(value, nextInput, progressIdx, backtrackedOpt) =>
              // rhs succeeds with no progress --> good, but keep old error for display because we haven't moved yet
              Result.Ok(value, nextInput, progressIdx, backtrackedOpt.map(_ `combine` error).orElse(Some(error)))
            case Result.Backtrack(error2, progressIdx) if progressIdx != origProgressIdx =>
              // rhs backtracking after progress --> give up (but record both errors)
              Result.Fatal(error.combine(error2), progressIdx)
            case Result.Backtrack(error2, progressIdx) =>
              // backtrack with no progress --> try again (recording both errors)
              Result.Backtrack(error.combine(error2), progressIdx)
            case result @ Result.Fatal(_, progressIdx) if progressIdx != origProgressIdx =>
              // rhs fatal after progress --> forward with no changes, our backtracking info is irrelevant
              result
            case Result.Fatal(error2, progressIdx) =>
              // rhs fatal with no progress --> still same position, add our error info
              Result.Fatal(error.combine(error2), progressIdx)
          }
        case Result.Fatal(error, progressIdx) => Eval.now(Result.Fatal(error, progressIdx))
      }
    }
  }

  @targetName("ignoreAndThen")
  final def ~>[U](other: =>P[U]): P[U] = {
    lazy val otherLazy = other
    self.flatMap(_ => otherLazy)
  }

  @targetName("andThenIgnore")
  final def <~(other: =>P[?]): P[T] = {
    lazy val otherLazy = other
    self.flatMap(value => otherLazy.map(_ => value))
  }

  @targetName("ignoreAndThenPeek")
  final def ~>?[U](other: =>P[U]): P[U] = {
    lazy val otherLazy = other
    self.flatMap(_ => otherLazy).peek
  }

  @targetName("andThenIgnorePeek")
  final def <~?(other: =>P[?]): P[T] = {
    lazy val otherLazy = other
    self.flatMap(value => otherLazy.map(_ => value)).peek
  }

  @targetName("andThen")
  final def ~[U](other: =>P[U]): P[T ~ U] = {
    lazy val otherLazy = other
    self.flatMap(leftValue => otherLazy.map(leftValue ~ _))
  }

  @targetName("andThenPeek")
  final def ~?[U](other: =>P[U]): P[T ~ U] = {
    lazy val otherLazy = other
    self.flatMap(leftValue => otherLazy.map(leftValue ~ _)).peek
  }
}

object Parser {
  enum Result[Input, Error, +T] {
    case Ok(value: T, nextInput: Input, progressIdx: Int, backtrackedOpt: Option[Error])
    case Backtrack(error: Error, progressIdx: Int)
    case Fatal(error: Error, progressIdx: Int)
  }

  final infix case class ~[+A, +B](a: A, b: B)
  extension [A](a: A) {
    @targetName("andThen")
    infix def ~[B](b: B): A ~ B = new ~(a, b)
  }

  final class Ops[Elem, Input, Error: Semigroup](using inputOps: InputOps[Elem, Input, Error])(using errorOps: ErrorOps[Elem, Error]) {
    given ops: Ops[Elem, Input, Error] = this // need this or we'll allocate too many new ops, including recursively
    import Parser.Result
    type P[T] = Parser[Elem, Input, Error, T]
    object P {
      def apply[T](fn: (Int, Input) => Eval[R[T]]): P[T] = new Parser[Elem, Input, Error, T] {
        override def apply(progressIdx: Int, input: Input): Eval[R[T]] = Eval.defer(fn(progressIdx, input))
      }
    }
    type R[T] = Parser.Result[Input, Error, T]

    export Parser.~

    given monad: StackSafeMonad[P] with {
      override def pure[A](x: A): P[A] = trivial(x)
      override def flatMap[A, B](fa: P[A])(f: A => P[B]): P[B] = fa.flatMap(f)
    }

    given semigroupK: SemigroupK[P] with {
      override def combineK[A](x: P[A], y: P[A]): P[A] = x | y
    }
    given semigroup[T]: Semigroup[P[T]] = semigroupK.algebra

    def peek[T](parser: P[T]): P[T] =
      parser.peek

    def trivial[T](value: T): P[T] =
      P { (progressIdx, input) => Eval.now(Result.Ok(value, input, progressIdx, None)) }

    val eof: P[Unit] =
      P { (progressIdx, input) =>
        inputOps.read(input) match {
          case Left(err) => Eval.now(Result.Fatal(err, progressIdx))
          case Right(None) => Eval.now(Result.Ok((), input, progressIdx, None))
          case Right(Some((actualElem, nextInput))) =>
            Eval.now(Result.Backtrack(
              errorOps.expectedEOF(
                sourceLocation = inputOps.lastSourceLocation(nextInput),
                actualElem = actualElem),
              progressIdx))
        }
      }

    val input: P[Input] =
      P { (progressIdx, input) => Eval.now(Result.Ok(input, input, progressIdx, None)) }

    val anyElem: P[Elem] =
      P { (progressIdx, input) =>
        inputOps.read(input) match {
          case Left(err) => Eval.now(Result.Fatal(err, progressIdx))
          case Right(None) =>
            Eval.now(Result.Backtrack(errorOps.unexpectedEOF(inputOps.nextSourceLocation(input)), progressIdx))
          case Right(Some((elem, nextInput))) =>
            Eval.now(Result.Ok(elem, nextInput, progressIdx + 1, None))
        }
      }

    def log[T](name: String)(parser: P[T]): P[T] =
      P { (progressIdx, input) =>
        val origProgressIdx = progressIdx
        println(s"[$name@$progressIdx] entering")
        parser(progressIdx, input).map {
          case res @ Result.Ok(value, nextInput, progressIdx, backtrackedOpt) =>
            println(s"[$name@$origProgressIdx] parsed $value, progressIdx=$progressIdx and backtrackedOpt=$backtrackedOpt")
            res
          case res @ Result.Backtrack(error, progressIdx) =>
            println(s"[$name@$origProgressIdx] backtrack $error, progressIdx=$progressIdx")
            res
          case res @ Result.Fatal(error, progressIdx) =>
            println(s"[$name@$origProgressIdx] fatal $error, progressIdx=$progressIdx")
            res
        }
      }

    def backtrack[T](error: Error): P[T] =
      P { (progressIdx, _) => Eval.now(Result.Backtrack(error, progressIdx)) }

    def phrase[T](body: P[T]): P[T] =
      body <~ eof

    def opt[T](elem: P[T]): P[Option[T]] =
      elem.map(Some(_)) | trivial(None)

    def rep[T](elem: P[T])(using sourcecode.Enclosing): P[Chain[T]] =
      (elem
        .assertProgress
        .flatMap(leftValue => rep(elem).map(leftValue +: _)))
      | trivial(Chain.empty)

    def rep1[T](elem: P[T])(using sourcecode.Enclosing): P[Chain[T]] =
      elem.flatMap(firstValue => rep(elem).map(firstValue +: _))

    def repsep[T](elem: P[T], sep: P[?])(using sourcecode.Enclosing): P[Chain[T]] =
      opt(elem).flatMap {
        case None => trivial(Chain.empty)
        case Some(firstValue) =>
          rep(sep ~>? elem).map(firstValue +: _)
      }

    def rep1sep[T](elem: P[T], sep: P[?])(using sourcecode.Enclosing): P[Chain[T]] =
      elem.flatMap(firstValue => rep(sep ~>? elem).map(firstValue +: _))

    def capturingPosition[T](parser: P[T]): P[(T, SourceLocation)] =
      for {
        prePos <- input.map(inputOps.nextSourceLocation)
        value <- parser
        postPos <- input.map(inputOps.lastSourceLocation)
      } yield (value, prePos.combine(postPos))
  }

  object Ops {
    def apply[Elem, Input, Error: Semigroup](using inputOps: InputOps[Elem, Input, Error])(using errorOps: ErrorOps[Elem, Error]): Ops[Elem, Input, Error] =
      new Ops
  }
}
