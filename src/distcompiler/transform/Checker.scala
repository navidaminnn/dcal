package distcompiler.transform

//import cats.*
//import cats.syntax.all.given

import java.time.{Instant, Duration}
import scala.collection.mutable
import Generator.Example

sealed trait Checker[T] { self =>
  import Checker.*
  type Self <: Checker[T]

  private[Checker] def streamChecker: StreamChecker[T]
  private[Checker] def replaceStreamChecker(streamChecker: StreamChecker[T]): Self { type Self = self.Self }

  final def requireDepth(depth: Int): Self =
    replaceStreamChecker {
      StreamChecker.combine(streamChecker, StreamChecker.requireDepth(depth))
    }

  final def transform[U](transFn: T => U)(checkerFn: TransformChecker[U] => TransformChecker[U]): Self { type Self = self.Self } =
    replaceStreamChecker {
      StreamChecker.combine(
        streamChecker,
        StreamChecker.transform(transFn, checkerFn(TransformChecker(StreamChecker.empty)).streamChecker),
      )
    }

  final def assertExists(pred: T => Boolean): Self { type Self = self.Self } =
    replaceStreamChecker {
      StreamChecker.combine(streamChecker, StreamChecker.assertExists(pred))
    }

  final def exists(pred: T => Boolean): Self { type Self = self.Self } =
    replaceStreamChecker {
      StreamChecker.combine(streamChecker, StreamChecker.exists(pred))
    }

  final def forall(fn: T => Unit): Self { type Self = self.Self } =
    replaceStreamChecker {
      StreamChecker.combine(streamChecker, StreamChecker.forall(fn))
    }
}

object Checker {
  def fromGenerator[T](gen: Generator[T]): RootChecker[T] =
    RootChecker(
      gen = gen,
      streamChecker = StreamChecker.empty,
      printExamples = true,
      disableANSI = false,
    )

  final case class RootChecker[T] private[Checker] (gen: Generator[T], streamChecker: StreamChecker[T],
                                                    printExamples: Boolean,
                                                    disableANSI: Boolean) extends Checker[T] {
    type Self = RootChecker[T]

    override def replaceStreamChecker(streamChecker: StreamChecker[T]): Self =
      copy(streamChecker = streamChecker)

    def withPrintExamples(printExamples: Boolean): RootChecker[T] =
      copy(printExamples = printExamples)

    def withDisableANSI(disableANSI: Boolean): RootChecker[T] =
      copy(disableANSI = disableANSI)

    def run(): Unit = {
      val startTime = Instant.now()

      // state space analytics
      var maxDepth = 0
      var countExplored = 0
      var countExploredSinceLast = 0
      var satisfiedAlready = false
      var lastExample: Option[Example[T]] = None
      var roundStart = Instant.now()
      var lastInterimReport = roundStart
      var totalCheckTime = Duration.ZERO

      extension (str: fansi.Str) def checkANSI: String =
        if(disableANSI) {
          str.plainText
        } else {
          str.render
        }

      def printExample(forcePrint: Boolean = false): Unit =
        if(lastExample.nonEmpty && (printExamples || forcePrint)) {
          print("  printing latest example: ")
          pprint.tokenize(lastExample.get, indent = 2)
            .map(_.checkANSI)
            .foreach(print)
          println()
        }

      def printTimeStats(): Unit =
        if(countExplored != 0 && countExploredSinceLast != 0) {
          print(s" taking avg ${humanDuration(totalCheckTime.dividedBy(countExplored))} to check examples.")
        }

      try {
        gen
          .examplesIterator
          .map {
            case example @ Example(_, depth) =>
              if(depth > maxDepth) {
                print(s"reached depth $depth: took ${humanDuration(Duration.between(roundStart, Instant.now()))} and covered ${humanNum(countExplored)} states so far (${humanNum(countExploredSinceLast)} since last msg).")
                printTimeStats()
                println()
                countExploredSinceLast = 0
                printExample()
                roundStart = Instant.now()
                lastInterimReport = roundStart
                maxDepth = depth

                if(streamChecker.isSatisfied) {
                  None
                } else {
                  Some(example)
                }
              } else {
                Some(example)
              }
          }
          .takeWhile(_.nonEmpty)
          .flatten
          .tapEach { _ =>
            if(countExploredSinceLast % 1000 == 0) {
              val now = Instant.now()
              if(Duration.between(lastInterimReport, now).toSeconds() > 30) {
                lastInterimReport = now
                print(s"  ... still exploring after ${humanDuration(Duration.between(roundStart, now))}. found ${humanNum(countExploredSinceLast)} examples this level.")
                printTimeStats()
                println()
                printExample()
              }
            }
            if(!satisfiedAlready && streamChecker.isSatisfied) {
              satisfiedAlready = true
              println(s"  ! found all we were looking for. finishing level...")
            }
          }
          .flatMap(_.flatten)
          .foreach { example =>
            lastExample = Some(example)
            countExplored += 1
            countExploredSinceLast += 1

            val checkStartTime = Instant.now()
            streamChecker.check(example)
            totalCheckTime = totalCheckTime.plus(Duration.between(checkStartTime, Instant.now()))
          }

        assert(streamChecker.isSatisfied, s"checking unsatisfied after exhausting all states")

        println {
          fansi.Color.Green(">>successful checking").checkANSI
          ++ s" after ${humanDuration(Duration.between(startTime, Instant.now()))}, after exploring ${humanNum(countExplored)} states."
        }
        printExample()
      } catch {
        case err =>
          println {
            fansi.Color.Red("!!found error").checkANSI
            ++ s" after ${humanDuration(Duration.between(startTime, Instant.now()))}, exploring ${humanNum(countExplored)} states."
          }
          printExample(forcePrint = true)
          throw err
      }
    }
  }

  final case class TransformChecker[T] private[Checker] (streamChecker: StreamChecker[T]) extends Checker[T] {
    type Self = TransformChecker[T]

    override def replaceStreamChecker(streamChecker: StreamChecker[T]): TransformChecker[T] =
      copy(streamChecker = streamChecker)
  }

  trait StreamChecker[T] {
    def check(example: Example[T]): Unit
    def isSatisfied: Boolean
    def isSatisfiedFinal: Boolean
  }

  object StreamChecker {
    def empty[T]: StreamChecker[T] =
      new StreamChecker[T] {
        def check(example: Example[T]): Unit = ()
        def isSatisfied: Boolean = true

        def isSatisfiedFinal: Boolean = true
      }

    def requireDepth[T](depth: Int): StreamChecker[T] =
      new StreamChecker[T] {
        var isSatisfied: Boolean = false
        def check(example: Example[T]): Unit =
          if(example.maxDepth >= depth) {
            isSatisfied = true
          }

        def isSatisfiedFinal: Boolean = isSatisfied
      }

    def forall[T](fn: T => Unit): StreamChecker[T] =
      new StreamChecker[T] {
        override def check(example: Example[T]): Unit = fn(example.value)
        override def isSatisfied: Boolean = true

        def isSatisfiedFinal: Boolean = true
      }

    def assertExists[T](pred: T => Boolean): StreamChecker[T] =
      new StreamChecker[T] {
        var isSatisfiedFinal = false

        def check(example: Example[T]): Unit =
          if(pred(example.value)) {
            isSatisfiedFinal = true
          }

        def isSatisfied: Boolean = true
      }

    def exists[T](pred: T => Boolean): StreamChecker[T] =
      new StreamChecker[T] {
        var isSatisfied = false
        def check(example: Example[T]): Unit = {
          if(pred(example.value)) {
            isSatisfied = true
          }
        }

        def isSatisfiedFinal: Boolean = isSatisfied
      }

    def combine[T](left: StreamChecker[T], right: StreamChecker[T]): StreamChecker[T] =
      new StreamChecker[T] {
        def check(example: Example[T]): Unit = {
          left.check(example)
          right.check(example)
        }
        def isSatisfied: Boolean =
          left.isSatisfied && right.isSatisfied

        def isSatisfiedFinal: Boolean =
          left.isSatisfiedFinal && right.isSatisfiedFinal
      }

    def transform[T, U](transFn: T => U, innerChecker: StreamChecker[U]): StreamChecker[T] =
      new StreamChecker[T] {
        def check(example: Example[T]): Unit = innerChecker.check(Example(transFn(example.value), example.maxDepth))
        def isSatisfied: Boolean = innerChecker.isSatisfied

        def isSatisfiedFinal: Boolean = innerChecker.isSatisfiedFinal
      }
  }

  private def humanNum(num: Int): String = {
    val str = num.toString()
    val incompletePrefix = str.take(str.size % 3)
    str
      .drop(str.size % 3)
      .grouped(3)
      .mkString(if(incompletePrefix.nonEmpty && str.size > 3) s"$incompletePrefix," else incompletePrefix, ",", "")
  }

  private def humanDuration(duration: Duration): String = {
    val builder = mutable.StringBuilder()
    def ensureSpace(): Unit =
      if(builder.nonEmpty) {
        builder ++= " "
      }

    if(duration.toHoursPart() != 0) {
      ensureSpace()
      builder ++= s"${duration.toHoursPart()}h"
    }
    if(duration.toMinutesPart() != 0) {
      ensureSpace()
      builder ++= s"${duration.toMinutesPart()}m"
    }
    if(duration.toSecondsPart() != 0) {
      ensureSpace()
      builder ++= s"${duration.toSecondsPart()}s"
    }
    if(duration.toMillisPart() != 0) {
      ensureSpace()
      builder ++= s"${duration.toMillisPart()}ms"
    }
    // only print nanoseconds if there's nothing else
    if(duration.toNanosPart() != 0 && builder.isEmpty) {
      ensureSpace()
      builder ++= s"${duration.toNanosPart()}ns"
    }

    assert(builder.nonEmpty)
    builder.result()
  }
}
