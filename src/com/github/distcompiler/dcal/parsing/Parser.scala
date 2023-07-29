package com.github.distcompiler.dcal.parsing

import scala.annotation.targetName
import cats.Eval
import cats.data.Chain

abstract class Parser[Elem, Input, Error, +T](using inputOps: InputOps[Elem, Input])(using errorOps: ErrorOps[Elem, Input, Error])(using ops: Parser.Ops[Elem, Input, Error]) { self =>
  import Parser.*
  import ops.*

  def apply(prevResult: R[?], input: Input): Eval[R[T]]

  final def parse(input: Input): Either[Error, (T, Input)] =
    self(Result.Trivial((), input), input).value match {
      case Result.Trivial(value, nextInput) => Right((value, nextInput))
      case Result.Progress(value, nextInput) => Right((value, nextInput))
      case Result.Backtrack(error) => Left(error)
      case Result.Fatal(error) => Left(error)
    }    

  final def parseAll(input: Input): Iterator[Either[Error, T]] =
    Iterator.unfold(Some(input): Option[Input]) {
      case None => None
      case Some(input) =>
        // parse EOF here, because that's the most reasonable way to check for EOF,
        // in which case we happily end parsing
        (self.left | eof.right).parse(input) match {
          case Left(error) => Some((Left(error), None)) // and this is why input is an option, so we can bail one step later
          case Right((Right(()), _)) => None
          case Right((Left(value), nextInput)) => Some((Right(value), Some(nextInput)))
        }
    }

  final def map[U](fn: T => U): P[U] =
    self.flatMap(value => trivial(fn(value)))

  final lazy val left: P[Left[T, Nothing]] = self.map(Left(_))

  final lazy val right: P[Right[Nothing, T]] = self.map(Right(_))

  final lazy val assertProgress: P[T] =
    P { (prevResult, input) =>
      self(prevResult, input).map {
        case Result.Trivial(_, _) =>
          throw AssertionError("Lack of progress detected! This would mean something like rep() is spinning in place.")
        case otherwise => otherwise
      }
    }

  final lazy val peek: P[T] =
    P { (prevResult, input) =>
      self(prevResult, input).map {
        case Result.Progress(value, nextInput) => Result.Trivial(value, nextInput)
        case otherwise => otherwise
      }
    }

  final def flatMap[U](fn: T => P[U]): P[U] =
    P { (prevResult, input) =>
      self(prevResult, input).flatMap {
        case Result.Trivial(value, nextInput) => fn(value)(prevResult, nextInput)
        case newPrevResult @ Result.Progress(value, nextInput) =>
          fn(value)(newPrevResult, nextInput)
        case Result.Backtrack(error) =>
          prevResult match {
            case Result.Progress(_, _) => Eval.now(Result.Fatal(error))
            case _ => Eval.now(Result.Backtrack(error))
          }
        case Result.Fatal(error) => Eval.now(Result.Fatal(error))
      }
    }

  final def constrain[U](fn: T => Either[Error, U]): P[U] =
    self.flatMap { value =>
      fn(value) match {
        case Left(error) => backtrack(error)
        case Right(value) => trivial(value)
      }
    }

  final def mapPositioned[T2, U](using ev: T <:< (T2, SourceLocation))(fn: SourceLocation ?=> T2 => U): P[U] =
    self.map { pair =>
      val (value, loc) = ev.apply(pair)
      fn(using loc)(value)
    }

  @targetName("or")
  final def |[U](other: =>P[U]): P[T | U] = {
    lazy val otherLazy = other
    P { (prevResult, input) =>
      // pass a trivial here because we are in a disjunction.
      // we'll stitch backtracking info back on if it matters
      // each disjunction should believe it has a clean slate, or else parsing more than one or two tokens will always fail
      self(Result.Trivial((), input), input).flatMap {
        case Result.Trivial(value, nextInput) =>
          Eval.now(Result.Trivial(value, nextInput))
        case Result.Progress(value, nextInput) =>
          Eval.now(Result.Progress(value, nextInput))
        case Result.Backtrack(error) =>
          val btError =
            prevResult match {
              case Result.Backtrack(prevError) =>
                errorOps.combine(prevError, error)
              case Result.Fatal(prevFatalError) =>
                throw AssertionError(s"We should never be backtracking from a fatal error! (err: $prevFatalError)")
              case _ => error
            }

          otherLazy(Result.Backtrack(btError), input)
        case Result.Fatal(error) => Eval.now(Result.Fatal(error))
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
    self.peek.flatMap(_ => otherLazy)
  }

  @targetName("andThenIgnorePeek")
  final def <~?(other: =>P[?]): P[T] = {
    lazy val otherLazy = other
    self.peek.flatMap(value => otherLazy.map(_ => value))
  }

  @targetName("andThen")
  final def ~[U](other: =>P[U]): P[T ~ U] = {
    lazy val otherLazy = other
    self.flatMap(leftValue => otherLazy.map(leftValue ~ _))
  }

  @targetName("andThenPeek")
  final def ~?[U](other: =>P[U]): P[T ~ U] = {
    lazy val otherLazy = other
    self.peek.flatMap(leftValue => otherLazy.map(leftValue ~ _))
  }
}

object Parser {
  enum Result[Input, Error, +T] {
    case Trivial(value: T, nextInput: Input)
    case Progress(value: T, nextInput: Input)
    case Backtrack(error: Error)
    case Fatal(error: Error)
  }

  @annotation.showAsInfix
  final case class ~[+A, +B](a: A, b: B)
  extension [A](a: A) {
    @targetName("andThen")
    infix def ~[B](b: B): A ~ B = new ~(a, b)
  }

  final class Ops[Elem, Input, Error](using inputOps: InputOps[Elem, Input])(using errorOps: ErrorOps[Elem, Input, Error]) {
    given Ops[Elem, Input, Error] = this // need this or the fields will try to allocate a new ops
    import Parser.Result
    type P[T] = Parser[Elem, Input, Error, T]
    object P {
      def apply[T](fn: (R[?], Input) => Eval[R[T]]): P[T] = new Parser[Elem, Input, Error, T] {
        override def apply(prevResult: R[?], input: Input): Eval[R[T]] = fn(prevResult, input)
      }
    }
    type R[T] = Parser.Result[Input, Error, T]

    export Parser.~

    def trivial[T](value: T): P[T] =
      P { (_, input) => Eval.now(Result.Trivial(value, input)) }

    val eof: P[Unit] =
      P { (_, input) =>
        inputOps.read(input) match {
          case None => Eval.now(Result.Trivial((), input))
          case Some((actualElem, _)) =>
            Eval.now(Result.Backtrack(errorOps.expectedEOF(input = input, actualElem = actualElem)))
        }
      }

    val input: P[Input] =
      P { (_, input) => Eval.now(Result.Trivial(input, input)) }

    val anyElem: P[Elem] =
      P { (_, input) =>
        inputOps.read(input) match {
          case None =>
            Eval.now(Result.Backtrack(errorOps.unexpectedEOF(input)))
          case Some((elem, nextInput)) =>
            Eval.now(Result.Progress(elem, nextInput))
        }
      }

    def backtrack[T](error: Error): P[T] =
      P { (_ , _) => Eval.now(Result.Backtrack(error)) }

    def phrase[T](body: P[T]): P[T] =
      body <~ eof

    def opt[T](elem: P[T]): P[Option[T]] =
      elem.map(Some(_)) | trivial(None)

    def rep[T](elem: P[T]): P[Chain[T]] =
      (elem
        .assertProgress
        .flatMap(leftValue => rep(elem).map(leftValue +: _)))
      | trivial(Chain.empty)

    def rep1[T](elem: P[T]): P[Chain[T]] =
      elem.flatMap(firstValue => rep(elem).map(firstValue +: _))

    def repsep[T](elem: P[T], sep: P[?]): P[Chain[T]] =
      opt(elem).flatMap {
        case None => trivial(Chain.empty)
        case Some(firstValue) =>
          rep(sep ~>? elem).map(firstValue +: _)
      }

    def rep1sep[T](elem: P[T], sep: P[?]): P[Chain[T]] =
      elem.flatMap(firstValue => rep(sep ~>? elem).map(firstValue +: _))

    def capturingPosition[T](parser: P[T])(using capturePrePosition: CapturePrePosition[Elem, Input]): P[(T, SourceLocation)] =
      for {
        prePos <- input.map(capturePrePosition.capture)
        value <- parser
        postPos <- input.map(inputOps.getPrevSourceLocation)
      } yield (value, prePos.combine(postPos))
  }

  object Ops {
    def apply[Elem, Input, Error](using inputOps: InputOps[Elem, Input])(using errorOps: ErrorOps[Elem, Input, Error]): Ops[Elem, Input, Error] =
      new Ops
  }

  trait CapturePrePosition[Elem, Input] {
    def capture(input: Input): SourceLocation
  }

  object CapturePrePosition {
    given capturePrePositionChar[Input](using inputOps: InputOps[Char, Input]): CapturePrePosition[Char, Input] with {
      override def capture(input: Input): SourceLocation = {
        val SourceLocation(path, startOffset, endOffset) = inputOps.getPrevSourceLocation(input)
        SourceLocation(path, startOffset + 1, endOffset + 1)
      }
    }

    given capturePrePositionSourceLocated[Elem <: SourceLocated, Input](using inputOps: InputOps[Elem, Input]): CapturePrePosition[Elem, Input] with {
      override def capture(input: Input): SourceLocation =
        inputOps.read(input)
          .map(_._1.sourceLocation)
          .getOrElse(inputOps.getPrevSourceLocation(input))
    }

    given capturePrePositionEitherSourceLocated[Error, Elem <: SourceLocated, Input](using inputOps: InputOps[Either[Error, Elem], Input]): CapturePrePosition[Either[Error, Elem], Input] with {
      override def capture(input: Input): SourceLocation =
        inputOps.read(input)
          .map(_._1)
          .flatMap(_.toOption)
          .map(_.sourceLocation)
          .getOrElse(inputOps.getPrevSourceLocation(input))
    }
  }
}
