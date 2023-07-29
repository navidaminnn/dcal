package com.github.distcompiler.dcal.parsing

import scala.annotation.targetName
import cats.Eval
import cats.data.Chain
import cats.Now
import cats.Later
import cats.Always

// import sourcecode.Enclosing

// final case class Eval[+T](value: T) {
//   def map[U](fn: T => U)(using enc: Enclosing): Eval[U] = {
//     println(s"in ${enc.value}")
//     Eval(fn(value))
//   }
//   def flatMap[U](fn: T => Eval[U])(using enc: Enclosing): Eval[U] = {
//     println(s"in ${enc.value}")
//     fn(value)
//   }
// }

// object Eval {
//   def now[T](value: T): Eval[T] = Eval(value)
// }

abstract class Parser[Elem, Input, Error, +T](using inputOps: InputOps[Elem, Input])(using errorOps: ErrorOps[Elem, Input, Error])(using ops: Parser.Ops[Elem, Input, Error]) { self =>
  import Parser.*
  import ops.*

  def apply(input: Input): Eval[R[T]]

  final def parse(input: Input): Either[Error, (T, Input)] =
    self(input).value match {
      case Result.Ok(value, nextInput, _, _) => Right((value, nextInput))
      case Result.Backtrack(error, _) => Left(error)
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
    P { input =>
      self(input).map {
        case Result.Ok(_, _, false, _) =>
          throw AssertionError("Lack of progress detected! This would mean something like rep() is spinning in place.")
        case otherwise => otherwise
      }
    }

  final lazy val peek: P[T] =
    P { input =>
      self(input).map {
        case Result.Ok(value, nextInput, _, backtrackedOpt) =>
          Result.Ok(value, nextInput, false, backtrackedOpt)
        case otherwise => otherwise
      }
    }

  final def flatMap[U](fn: T => P[U]): P[U] =
    P { input =>
      self(input).flatMap {
        case Result.Ok(value, nextInput, madeProgress1, backtrackedOpt1) =>
          fn(value)(nextInput).map {
            case Result.Ok(value, nextInput, madeProgress2, backtrackedOpt2) =>
              val madeProgress = madeProgress1 || madeProgress2
              Result.Ok(value, nextInput,
                madeProgress = madeProgress,
                backtrackedOpt = if(madeProgress2) {
                  None
                } else {
                  (backtrackedOpt1.toList ++ backtrackedOpt2.toList)
                    .reduceOption(errorOps.combine)
                })
            case Result.Backtrack(error, floating) if madeProgress1 =>
              if(floating) {
                Result.Backtrack(error, false)
              } else {
                Result.Fatal(error)
              }
            case otherwise => otherwise
          }
        case Result.Backtrack(error, floating) => Eval.now(Result.Backtrack(error, floating))
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
    P { input =>
      self(input).flatMap {
        case Result.Ok(value, nextInput, madeProgress, backtrackedOpt) =>
          Eval.now(Result.Ok(value, nextInput, madeProgress, backtrackedOpt))
        case Result.Backtrack(error, floating) =>
          otherLazy(input).map {
            case Result.Ok(value, nextInput, madeProgress, backtrackedOpt) =>
              Result.Ok(value, nextInput, madeProgress, (backtrackedOpt.toList ++ List(error)).reduceOption(errorOps.combine))
            case Result.Backtrack(error2, floating2) => Result.Backtrack(errorOps.combine(error, error2), floating2)
            case otherwise => otherwise
          }
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
    case Ok(value: T, nextInput: Input, madeProgress: Boolean, backtrackedOpt: Option[Error])
    case Backtrack(error: Error, floating: Boolean)
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
      def apply[T](fn: Input => Eval[R[T]]): P[T] = new Parser[Elem, Input, Error, T] {
        override def apply(input: Input): Eval[R[T]] = fn(input)
      }
    }
    type R[T] = Parser.Result[Input, Error, T]

    export Parser.~

    def trivial[T](value: T): P[T] =
      P { input => Eval.now(Result.Ok(value, input, false, None)) }

    val eof: P[Unit] =
      P { input =>
        inputOps.read(input) match {
          case None => Eval.now(Result.Ok((), input, false, None))
          case Some((actualElem, _)) =>
            Eval.now(Result.Backtrack(errorOps.expectedEOF(input = input, actualElem = actualElem), false))
        }
      }

    val input: P[Input] =
      P { input => Eval.now(Result.Ok(input, input, false, None)) }

    val anyElem: P[Elem] =
      P { input =>
        inputOps.read(input) match {
          case None =>
            Eval.now(Result.Backtrack(errorOps.unexpectedEOF(input), false))
          case Some((elem, nextInput)) =>
            Eval.now(Result.Ok(elem, nextInput, true, None))
        }
      }

    def log[T](name: String)(parser: P[T]): P[T] =
      P { input =>
        println(s"[$name] entering")
        parser(input).map {
          case res @ Result.Ok(value, nextInput, madeProgress, backtrackedOpt) =>
            println(s"[$name] parsed $value, madeProgress=$madeProgress and backtrackedOpt=$backtrackedOpt")
            res
          case res @ Result.Backtrack(error, floating) =>
            println(s"[$name] backtrack $error, floating=$floating")
            res
          case res @ Result.Fatal(error) =>
            println(s"[$name] fatal $error")
            res
        }
      }

    def backtrack[T](error: Error): P[T] =
      P { _ => Eval.now(Result.Backtrack(error, true)) }

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
