package distcompiler.parsing

import cats.*
import cats.data.*
import cats.syntax.all.given

import scala.annotation.targetName
import scala.collection.mutable

import distcompiler.util.EvalList

enum Parser[Elem, Input, Error, +T] {
  private type P[+T] = Parser[Elem, Input, Error, T]

  val handle: Parser.Handle = Parser.Handle()

  case ReadElem() extends Parser[Elem, Input, Error, Elem]
  case AtEof() extends Parser[Elem, Input, Error, Unit]
  case Input() extends Parser[Elem, Input, Error, Input]
  case Trivial(value: T)
  case AssertProgress(p: Parser[Elem, Input, Error, T], assertion: Eval[Unit])
  case Peek(p: Parser[Elem, Input, Error, T])
  case Log(name: String, p: Parser[Elem, Input, Error, T])
  case Lazy(pFn: () => Parser[Elem, Input, Error, T])
  case Constrain[Elem, Input, Error, T, U](p: Parser[Elem, Input, Error, T], pred: T => Either[Error, U]) extends Parser[Elem, Input, Error, U]
  case Or[Elem, Input, Error, T, U](left: Parser[Elem, Input, Error, T], right: Parser[Elem, Input, Error, U]) extends Parser[Elem, Input, Error, T | U]
  case OrLongest[Elem, Input, Error, T, U](left: Parser[Elem, Input, Error, T], right: Parser[Elem, Input, Error, U]) extends Parser[Elem, Input, Error, T | U]
  case Ap[Elem, Input, Error, T, U](ff: Parser[Elem, Input, Error, T => U], fa: Parser[Elem, Input, Error, T]) extends Parser[Elem, Input, Error, U]
  case AndThen[Elem, Input, Error, T, U](pt: Parser[Elem, Input, Error, T], fn: T => Parser[Elem, Input, Error, U]) extends Parser[Elem, Input, Error, U]

  def mustMakeProgress: Boolean =
    assertProgressImpl.value

  private def assertProgressImpl: Eval[Boolean] = {
    val cache = mutable.HashMap[Parser.Handle, Boolean]()
    def impl[T](self: P[T]): Eval[Boolean] = {
      def compute: Eval[Boolean] =
        self match {
          case ReadElem() => Eval.True
          case AtEof() => Eval.False
          case Input() => Eval.False
          case Trivial(_) => Eval.False
          case AssertProgress(p, _) => Eval.defer(impl(p))
          case Peek(p) => Eval.defer(impl(p))
          case Log(name, p) => Eval.defer(impl(p))
          case lz @ Lazy(pFn) =>
            cache.put(lz.handle, false)
            Eval.defer(impl(pFn()))
          case Constrain(p, _) => Eval.defer(impl(p))
          case Or(left, right) =>
            // don't need short-circuit so much here because if it's false we'll crash anyway
            (Eval.defer(impl(left)), Eval.defer(impl(right))).mapN(_ && _)
          case OrLongest(left, right) =>
            (Eval.defer(impl(left)), Eval.defer(impl(right))).mapN(_ && _)
          case Ap(ff, fa) => 
            // short-circuit to save work (bias to the "early" side so we stop at the "first" progress and ignore irrelevant later manipulations)
            Eval.defer(impl(ff)).flatMap {
              case true => Eval.True
              case false => impl(fa)
            }
          case AndThen(pt, _) => Eval.defer(impl(pt))
        }

      cache.get(self.handle) match {
        case None =>
          compute.map { result =>
            cache.put(self.handle, result)
            result
          }
        case Some(result) =>
          Eval.now(result)
      }
    }

    impl(this)
  }

  def assertProgress: P[T] =
    AssertProgress(this, assertProgressImpl.memoize.map { mustMakeProgress =>
      assert(mustMakeProgress, "lack of progress detected! (could not prove that the parser won't get stuck)")
    })

  def log(name: String): P[T] =
    Log(name, this)

  def parse(input: Input)(using inputOps: InputOps[Elem, Input, Error], errorOps: ErrorOps[Elem, Error]): Either[NonEmptyList[Error], (T, Input)] = {
    type Anchor = (Parser.Handle, Int)

    enum Result[+T] derives CanEqual {
      case Ok(value: T, nextInput: Input, recoveredErrors: Chain[Error], anchorTrace: Set[Anchor])
      case Backtrack(errors: Chain[Error], idx: Int) extends Result[Nothing]
      case Fatal(errors: Chain[Error], idx: Int) extends Result[Nothing]
    }

    extension (input: Input) def idx: Int = inputOps.tellIdx(input)

    val leftRecTable = mutable.HashMap[Anchor, Option[Any]]()
    val leftRecReached = mutable.HashSet[Anchor]()

    val cache = java.util.WeakHashMap[Parser.Handle, (Int, Result[?])]()

    def impl[T](self: P[T], input: Input): Eval[Result[T]] = {
      def compute: Eval[Result[T]] =
        self match {
          case ReadElem() =>
            inputOps.read(input) match {
              case Left(error) =>
                Eval.now(Result.Fatal(Chain.one(error), input.idx))
              case Right(Some(elem, nextInput)) =>
                Eval.now(Result.Ok(elem, nextInput, Chain.nil, Set.empty)) 
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
                Eval.now(Result.Ok((), input, Chain.nil, Set.empty))
            }
          case Input() => 
            Eval.now(Result.Ok(input, input, Chain.nil, Set.empty))
          case Trivial(value) =>
            Eval.now(Result.Ok(value, input, Chain.nil, Set.empty))
          case AssertProgress(p, assertion) =>
            assertion.productR(Eval.defer(impl(p, input)))
          case Peek(p) =>
            Eval.defer(impl(p, input)).map {
              case result @ Result.Ok(_, _, _, _) => result
              case Result.Backtrack(errors, _) => Result.Backtrack(errors, input.idx)
              case result @ Result.Fatal(_, _) => result
            }
          case Log(name, p) =>
            println(s"[$name @ ${input.idx}] enter")
            Eval.defer(impl(p, input)).map { result =>
              result match {
                case Result.Ok(value, nextInput, _, _) =>
                  println(s"[$name ${input.idx}] success w/ $value ending at ${nextInput.idx}")
                case Result.Backtrack(errors, idx) =>
                  println(s"[$name ${input.idx}] backtrack @ $idx")
                case Result.Fatal(errors, idx) =>
                  println(s"[$name ${input.idx}] fatal @ $idx")
              }
              result
            }
          case Lazy(pFn) =>
            val handle = self.handle
            val pp = pFn()
            val anchor = (handle, input.idx)
            leftRecTable.get(anchor) match {
              case None =>
                leftRecTable.update(anchor, None)
                leftRecReached += anchor

                def grow(result: Result[T], smallerValue: Option[T], prevInput: Input, prevRecoveredErrors: Chain[Error], anchor: Anchor, prevAnchorTrace: Set[Anchor]): Eval[Result[T]] =
                  result match {
                    case result @ Result.Ok(value, nextInput, recoveredErrors, anchorTrace) =>
                      if(leftRecReached(anchor) || anchorTrace(anchor)) {
                        leftRecTable.remove(anchor)
                        leftRecReached.remove(anchor)

                        val nextAnchor = (handle, nextInput.idx)
                        leftRecTable.update(nextAnchor, Some(value))

                        // this is not always true, for instance in the case of rep().
                        // we can consume no input after failing a recursive case, at which point
                        // we will attempt to grow, see that we get a trivial non-recursive input again
                        // (without leftRecReached this time), and end with our trivial output
                        if(anchorTrace(anchor)) {
                          // instead, assert it's true for the growth case _where we reached a rec case and consumed no input anyway_
                          // ... that would never be reasonable, would it?
                          assert(prevInput.idx < nextInput.idx)
                        }
                        Eval.defer(impl(pp, nextInput))
                          .flatMap(grow(_, Some(value), nextInput, recoveredErrors, nextAnchor, prevAnchorTrace ++ anchorTrace.excl(anchor)))
                          .productL(Eval.always {
                            leftRecReached.remove(anchor)
                            leftRecTable.remove(nextAnchor)
                          })
                      } else {
                        // if we didn't reach ourselves recursively but did get a value, it means we used a non-rec case.
                        // don't return that, because we shouldn't have tried that from this position. just pretend
                        // we stopped at the prev value
                        smallerValue match {
                          case None => Eval.now(result.copy(anchorTrace = prevAnchorTrace ++ anchorTrace))
                          case Some(value) => 
                            Eval.now(Result.Ok(value, prevInput, prevRecoveredErrors, prevAnchorTrace ++ anchorTrace.excl(anchor)))
                        }
                      }
                    case Result.Backtrack(errors, _) =>
                      smallerValue match {
                        case None =>
                          Eval.now(result)
                        case Some(value) =>
                          Eval.now(Result.Ok(value, prevInput, recoveredErrors = errors, prevAnchorTrace))
                      }
                    case result @ Result.Fatal(_, _) => Eval.now(result)
                  }

                Eval.defer(impl(pp, input))
                  .flatMap(grow(_, None, input, Chain.nil, anchor, Set.empty))
                  .productL(Eval.always { 
                    leftRecReached.remove(anchor)
                    leftRecTable.remove(anchor)
                  })
              case Some(None) =>
                leftRecReached += anchor
                Eval.now(Result.Backtrack(Chain.nil, input.idx))
              case Some(Some(value)) =>
                Eval.now(Result.Ok(value.asInstanceOf[T], input, Chain.nil, Set(anchor)))
            }
          case Constrain(p, pred) =>
            Eval.defer(impl(p, input)).map {
              case result @ Result.Ok(value, nextInput, recoveredErrors, _) =>
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
                    result.copy(value = value)
                }
              case result @ (Result.Backtrack(_, _) | Result.Fatal(_, _)) => result
            }
          case self @ (Or(_, _) | OrLongest(_, _)) =>
            val (left, right, keepLongest) = self match {
              case Or(left, right) => (left, right, false)
              case OrLongest(left, right) => (left, right, true)
            }

            Eval.defer(impl(left, input)).flatMap {
              case result @ Result.Ok(_, _, recoveredErrors, _) =>
                if(keepLongest) {
                  val outerErrors = recoveredErrors
                  Eval.defer(impl(right, input)).map {
                    case result2 @ Result.Ok(_, _, recoveredErrors, _) =>
                      val chosenResult = 
                        if(result.nextInput.idx < result2.nextInput.idx) {
                          result2
                        } else {
                          result
                        }
                      if(result.nextInput.idx == result2.nextInput.idx) {
                        chosenResult.copy(recoveredErrors = outerErrors ++ recoveredErrors)
                      } else {
                        chosenResult
                      }
                    case Result.Backtrack(errors, idx) if idx == input.idx =>
                      result.copy(recoveredErrors = outerErrors ++ errors)
                    case Result.Backtrack(errors, idx) =>
                      Result.Fatal(errors, idx)
                    case result @ Result.Fatal(_, _) => result
                  }
                } else {
                  Eval.now(result)
                }
              case Result.Backtrack(errors, idx) if idx == input.idx =>
                val inputIdx = input.idx
                val outerErrors = errors
                Eval.defer(impl(right, input)).map {
                  case result @ Result.Ok(value, nextInput, recoveredErrors, _) if nextInput.idx == idx => 
                    result.copy(recoveredErrors = outerErrors ++ recoveredErrors)
                  case result @ Result.Backtrack(errors, `inputIdx`) =>
                    result.copy(errors = outerErrors ++ errors)
                  case Result.Backtrack(errors, idx) =>
                    Result.Fatal(errors, idx)
                  case result @ Result.Fatal(errors, `inputIdx`) =>
                    result.copy(errors = outerErrors ++ errors)
                  case result => result
                }
              case Result.Backtrack(errors, idx) =>
                Eval.now(Result.Fatal(errors, idx))
              case result @ Result.Fatal(_, _) =>
                Eval.now(result)
            }
          case Ap(ff, fa) =>
            Eval.defer(impl(ff, input)).flatMap {
              case Result.Ok(ff, nextInput, recoveredErrors, anchorTrace) =>
                val outerIdx = nextInput.idx
                val outerErrors = recoveredErrors
                val outerAnchorTrace = anchorTrace
                Eval.defer(impl(fa, nextInput)).map {
                  case result @ Result.Ok(fa, nextInput, recoveredErrors, anchorTrace) if nextInput.idx == outerIdx =>
                    result.copy(
                      value = ff(fa),
                      recoveredErrors = outerErrors ++ recoveredErrors,
                      anchorTrace = outerAnchorTrace ++ anchorTrace)
                  case result @ Result.Ok(fa, _, _, anchorTrace) =>
                    result.copy(value = ff(fa), anchorTrace = outerAnchorTrace ++ anchorTrace)
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
            Eval.defer(impl(pt, input)).flatMap {
              case Result.Ok(t, nextInput, recoveredErrors, anchorTrace) =>
                val outerIdx = nextInput.idx
                val outerErrors = recoveredErrors
                val outerAnchorTrace = anchorTrace
                Eval.defer(impl(fn(t), nextInput)).map {
                  case result @ Result.Ok(_, _, recoveredErrors, anchorTrace) if nextInput.idx == outerIdx =>
                    result.copy(
                      recoveredErrors = outerErrors ++ recoveredErrors,
                      anchorTrace = outerAnchorTrace ++ anchorTrace)
                  case result @ Result.Ok(_, _, _, anchorTrace) =>
                    result.copy(anchorTrace = outerAnchorTrace ++ anchorTrace)
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
        }
      
      cache.get(self.handle) match {
        case (idx, result) if idx == input.idx =>
          Eval.now(result.asInstanceOf[Result[T]])
        case notThere =>
          if(notThere ne null) {
            cache.remove(self.handle)
          }
          compute.map { result =>
            cache.put(self.handle, (input.idx, result))
            result
          }
      }
      
    }
      
    impl(this, input).value match {
      case Result.Ok(value, nextInput, _, _) =>
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

  def as[U](u: U): P[U] =
    this.map(_ => u)

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

  def andThen[U](fn: T => P[U]): P[U] =
    AndThen(this, fn)

  def constrain[U](pred: T => Either[Error, U]): P[U] =
    Constrain(this, pred)

  @targetName("or")
  def |[U](other: P[U]): P[T | U] =
    Or(this, other)

  @targetName("orLongest")
  def |||[U](other: P[U]): P[T | U] =
    OrLongest(this, other)
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

    def phrase[T](body: P[T]): P[T] =
      body <~ eof

    def opt[T](elem: P[T]): P[Option[T]] =
      elem.map(Some(_)) | trivial(None)

    def lzy[T](parser: =>P[T]): P[T] = {
      lazy val parserLzy = parser
      Parser.Lazy(() => parserLzy)
    }

    def rep[T](elem: P[T])(using sourcecode.Enclosing): P[Chain[T]] =
      localRec { rec =>
        (elem.assertProgress, rec).mapN(_ +: _)
        | trivial(Chain.empty)
      }

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

    def localRec[T](fn: P[T] => P[T]): P[T] = {
      lazy val pp: P[T] = lzy(fn(pp))
      pp
    }

    final class PrecedenceTree[K: PartialOrder, T](pairs: Chain[PrecedenceTree.Case[K, T]], minPrecedence: Chain[P[T]]) {
      import PrecedenceTree.Case

      def level(precedence: K)(fn: P[T] => P[T]): PrecedenceTree[K, T] =
        PrecedenceTree(
          pairs = pairs :+ Case.Level(precedence, fn),
          minPrecedence = minPrecedence)

      def levels(lvls: IterableOnce[(K, P[T] => P[T])]): PrecedenceTree[K, T] =
        lvls.iterator.foldLeft(this)((acc, pair) => acc.level(pair._1)(pair._2))

      def levelRec(precedence: K)(fn: P[T] => P[T] => P[T]): PrecedenceTree[K, T] =
        PrecedenceTree(
          pairs = pairs :+ Case.LevelRec(precedence, fn),
          minPrecedence = minPrecedence)

      def levelAssoc(precedence: K)(fn: P[T] => P[T] => P[T]): PrecedenceTree[K, T] =
        PrecedenceTree(
          pairs = pairs :+ Case.LevelAssoc(precedence, fn),
          minPrecedence = minPrecedence)

      def levelsRec(lvls: IterableOnce[(K, P[T] => P[T] => P[T])]): PrecedenceTree[K, T] =
        lvls.iterator.foldLeft(this)((acc, pair) => acc.levelRec(pair._1)(pair._2))

      def levelsAssoc(lvls: IterableOnce[(K, P[T] => P[T] => P[T])]): PrecedenceTree[K, T] =
        lvls.iterator.foldLeft(this)((acc, pair) => acc.levelAssoc(pair._1)(pair._2))

      def bottomLevel(p: P[T]): PrecedenceTree[K, T] =
        PrecedenceTree(
          pairs = pairs,
          minPrecedence = minPrecedence :+ p)

      lazy val parser: P[T] = {
        val sortedPairs =
          pairs
            .toList
            .sortBy(_.precedence)(using Ordering.fromLessThan[K](_ < _))
        val bottomParser = {
          assert(minPrecedence.nonEmpty)
          minPrecedence.iterator.reduce(_ | _)
        }

        val cache = mutable.HashMap[K, P[T]]()

        def impl(pairs: List[Case[K, T]]): P[T] =
          pairs match {
            case Nil => bottomParser
            case cse :: restPairs =>
              cache.getOrElseUpdate(cse.precedence, {
                def withFn(cse: Case[K, T])(body: (P[T] => P[T]) => P[T]): P[T] =
                  cse match {
                    case Case.Level(_, fn) => body(fn)
                    case Case.LevelRec(_, fn) => localRec(rec => body(fn(rec)))
                    case Case.LevelAssoc(_, fn) =>
                      body { higher =>
                        localRec { rec =>
                          fn(rec | higher)(higher)
                        }
                      }
                  }

                def findHigher(pairs: List[Case[K, T]], root: K)(using keysIncluded: mutable.ArrayBuffer[K]): P[T] =
                  pairs match {
                    case Nil => bottomParser
                    case cse :: restPairs =>
                      if(keysIncluded.forall(cse.precedence > _)) {
                        impl(pairs)
                      } else if(keysIncluded.exists(cse.precedence > _) || !(cse.precedence > root)) {
                        findHigher(restPairs, root)
                      } else {
                        keysIncluded += cse.precedence
                        withFn(cse) { fn =>
                          fn(findHigher(restPairs, cse.precedence)(using mutable.ArrayBuffer(cse.precedence)))
                          | findHigher(restPairs, root)
                        }
                      }
                  }
                
                withFn(cse) { fn =>
                  fn(findHigher(restPairs, cse.precedence)(using mutable.ArrayBuffer(cse.precedence)))
                  | impl(restPairs)
                }
              })
          }
        
        impl(sortedPairs)
      }
    }

    object PrecedenceTree {
      enum Case[K, T] {
        case Level(precedence: K, fn: P[T] => P[T])
        case LevelRec(precedence: K, fn: P[T] => P[T] => P[T])
        case LevelAssoc(precedence: K, fn: P[T] => P[T] => P[T])

        def precedence: K
      }
    }

    def precedenceTree[K: PartialOrder: Hash, T]: PrecedenceTree[K, T] = PrecedenceTree(Chain.nil, Chain.nil)
  }
  
  object Ops {
    def apply[Elem, Input, Error]: Ops[Elem, Input, Error] =
      new Ops
  }
}
