package test.com.github.distcompiler.dcal.chungus

import java.time.{Instant, Duration}
import scala.collection.mutable
import scala.annotation.targetName
import fansi.Str

sealed trait Checker[T] { self =>
  import Checker.*
  type Self <: Checker[T]

  private[chungus] def streamChecker: StreamChecker[T]
  private[chungus] def replaceStreamChecker(streamChecker: StreamChecker[T]): Self

  final def transform[U](transFn: T => U)(checkerFn: TransformChecker[U] => TransformChecker[U]): Self =
    replaceStreamChecker {
      StreamChecker.transform(transFn, checkerFn(TransformChecker(StreamChecker.empty)).streamChecker)
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
      allowedLevelsWithNoExamples = 2,
      disableANSI = false,
    )

  final case class RootChecker[T] private[chungus] (gen: Generator[T], streamChecker: StreamChecker[T],
                                                    printExamples: Boolean,
                                                    allowedLevelsWithNoExamples: Int,
                                                    disableANSI: Boolean) extends Checker[T] {
    type Self = RootChecker[T]

    override def replaceStreamChecker(streamChecker: StreamChecker[T]): RootChecker[T] =
      copy(streamChecker = streamChecker)

    def withPrintExamples(printExamples: Boolean): RootChecker[T] =
      copy(printExamples = printExamples)

    def withAllowedLevelsWithNoExamples(allowedLevelsWithNoExamples: Int): RootChecker[T] =
      copy(allowedLevelsWithNoExamples = allowedLevelsWithNoExamples)

    def withDisableANSI(disableANSI: Boolean): RootChecker[T] =
      copy(disableANSI = disableANSI)

    def run(): Unit = {
      val startTime = Instant.now()

      // state space analytics
      var countExplored = 0
      var countExploredSinceLast = 0
      var consecutiveEmptyLevelsSinceLast = 0
      var lastExample: Option[T] = None
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

      var depth = 0
      try {
        Iterator.unfold(()) { _ =>
          if(depth > 0) {
            if(countExploredSinceLast == 0) {
              consecutiveEmptyLevelsSinceLast += 1
            } else {
              consecutiveEmptyLevelsSinceLast = 0
            }

            println(s"explored depth ${depth - 1}: took ${humanDuration(Duration.between(roundStart, Instant.now()))} and covered $countExplored states so far ($countExploredSinceLast since last msg)")
            countExploredSinceLast = 0
            printExample()
          }
          Predef.assert(consecutiveEmptyLevelsSinceLast < allowedLevelsWithNoExamples,
            s"""went $consecutiveEmptyLevelsSinceLast levels without finding any examples.
              |Very suspicious! Your generator might have run out of examples.
              |allowedLevelsWithNoExamples = $allowedLevelsWithNoExamples; change this if you need more permissive behavior""".stripMargin)
          
          if(!streamChecker.isSatisfied) {
            val depthToGen = depth
            depth += 1
            roundStart = Instant.now()
            lastInterimReport = roundStart
            Some(gen.computeResultsForDepth(depth = depthToGen), ())
          } else {
            None
          }
        }
        .flatten
        .tapEach { _ =>
          if(countExploredSinceLast % 1000 == 0) {
            val now = Instant.now()
            if(Duration.between(lastInterimReport, now).toSeconds() > 30) {
              lastInterimReport = now
              println(s"  ... still exploring depth ${depth-1} after ${humanDuration(Duration.between(roundStart, now))}. found $countExploredSinceLast examples this level.")
              printExample()
            }
          }
        }
        .flatten
        .tapEach(example => lastExample = Some(example))
        .tapEach(streamChecker.check)
        .foreach { _ =>
          countExplored += 1
          countExploredSinceLast += 1
        }
          
        Predef.assert(countExplored > 0, s"checked no values - that's usually bad")
        println {
          fansi.Color.Green(">>successful checking").checkANSI
          ++ s" after ${humanDuration(Duration.between(startTime, Instant.now()))}, after exploring $countExplored states."
        }
      } catch {
        case err =>
          println {
            fansi.Color.Red("!!found error").checkANSI
            ++ s" after ${humanDuration(Duration.between(startTime, Instant.now()))}, exploring $countExplored states."
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
    def check(value: T): Unit
    def isSatisfied: Boolean
  }

  object StreamChecker {
    def empty[T]: StreamChecker[T] =
      new StreamChecker[T] {
        override def check(value: T): Unit = ()
        override def isSatisfied: Boolean = true
      }

    def forall[T](fn: T => Unit): StreamChecker[T] =
      new StreamChecker[T] {
        override def check(value: T): Unit = fn(value)
        override def isSatisfied: Boolean = true
      }

    def exists[T](pred: T => Boolean): StreamChecker[T] =
      new StreamChecker[T] {
        var isSatisfied = false
        override def check(value: T): Unit = {
          if(pred(value)) {
            isSatisfied = true
          }
        }
      }

    def combine[T](left: StreamChecker[T], right: StreamChecker[T]): StreamChecker[T] =
      new StreamChecker[T] {
        override def check(value: T): Unit = {
          left.check(value)
          right.check(value)
        }
        override def isSatisfied: Boolean =
          left.isSatisfied && right.isSatisfied
      }

    def transform[T, U](transFn: T => U, innerChecker: StreamChecker[U]): StreamChecker[T] =
      new StreamChecker[T] {
        override def check(value: T): Unit = innerChecker.check(transFn(value))
        override def isSatisfied: Boolean = innerChecker.isSatisfied
      }
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
