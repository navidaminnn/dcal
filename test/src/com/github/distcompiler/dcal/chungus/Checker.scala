package test.com.github.distcompiler.dcal.chungus

import java.time.{Instant, Duration}
import scala.collection.mutable
import scala.annotation.targetName
import fansi.Str
import Generator.Example

sealed trait Checker[T] { self =>
  import Checker.*
  type Self <: Checker[T]

  private[chungus] def streamChecker: StreamChecker[T]
  private[chungus] def replaceStreamChecker(streamChecker: StreamChecker[T]): Self

  final def requireDepth(depth: Int): Self =
    replaceStreamChecker {
      StreamChecker.combine(streamChecker, StreamChecker.requireDepth(depth))
    }

  final def transform[U](transFn: T => U)(checkerFn: TransformChecker[U] => TransformChecker[U]): Self =
    replaceStreamChecker {
      StreamChecker.combine(
        streamChecker,
        StreamChecker.transform(transFn, checkerFn(TransformChecker(StreamChecker.empty)).streamChecker),
      )
    }

  final def exists(pred: T => Boolean): Self =
    replaceStreamChecker {
      StreamChecker.combine(streamChecker, StreamChecker.exists(pred))
    }

  final def forall(fn: T => Unit): Self =
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

  final case class RootChecker[T] private[chungus] (gen: Generator[T], streamChecker: StreamChecker[T],
                                                    printExamples: Boolean,
                                                    disableANSI: Boolean) extends Checker[T] {
    type Self = RootChecker[T]

    override def replaceStreamChecker(streamChecker: StreamChecker[T]): RootChecker[T] =
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

      try {
        gen
          .examplesIterator
          .map {
            case example @ Example(_, depth) =>
              if(depth > maxDepth) {
                println(s"reached depth $depth: took ${humanDuration(Duration.between(roundStart, Instant.now()))} and covered ${humanNum(countExplored)} states so far (${humanNum(countExploredSinceLast)} since last msg)")
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
                println(s"  ... still exploring after ${humanDuration(Duration.between(roundStart, now))}. found ${humanNum(countExploredSinceLast)} examples this level.")
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

            streamChecker.check(example)
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

  final case class TransformChecker[T] private[chungus] (streamChecker: StreamChecker[T]) extends Checker[T] {
    type Self = TransformChecker[T]

    override def replaceStreamChecker(streamChecker: StreamChecker[T]): TransformChecker[T] =
      copy(streamChecker = streamChecker)
  }

  trait StreamChecker[T] {
    def check(example: Example[T]): Unit
    def isSatisfied: Boolean
  }

  object StreamChecker {
    def empty[T]: StreamChecker[T] =
      new StreamChecker[T] {
        override def check(example: Example[T]): Unit = ()
        override def isSatisfied: Boolean = true
      }

    def requireDepth[T](depth: Int): StreamChecker[T] =
      new StreamChecker[T] {
        var isSatisfied: Boolean = false
        override def check(example: Example[T]): Unit =
          if(example.maxDepth >= depth) {
            isSatisfied = true
          }
      }

    def forall[T](fn: T => Unit): StreamChecker[T] =
      new StreamChecker[T] {
        override def check(example: Example[T]): Unit = fn(example.value)
        override def isSatisfied: Boolean = true
      }

    def exists[T](pred: T => Boolean): StreamChecker[T] =
      new StreamChecker[T] {
        var isSatisfied = false
        override def check(example: Example[T]): Unit = {
          if(pred(example.value)) {
            isSatisfied = true
          }
        }
      }

    def combine[T](left: StreamChecker[T], right: StreamChecker[T]): StreamChecker[T] =
      new StreamChecker[T] {
        override def check(example: Example[T]): Unit = {
          left.check(example)
          right.check(example)
        }
        override def isSatisfied: Boolean =
          left.isSatisfied && right.isSatisfied
      }

    def transform[T, U](transFn: T => U, innerChecker: StreamChecker[U]): StreamChecker[T] =
      new StreamChecker[T] {
        override def check(example: Example[T]): Unit = innerChecker.check(Example(transFn(example.value), example.maxDepth))
        override def isSatisfied: Boolean = innerChecker.isSatisfied
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
