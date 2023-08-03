package test.com.github.distcompiler.dcal.chungus

import java.time.{Instant, Duration}
import scala.collection.mutable
import scala.annotation.targetName
import fansi.Str

sealed trait Checker[T] { self =>
  def apply(gen: Generator[T]): Unit
}

object Checker {
  final case class Example[T](value: T, budget: Int)

  final case class PosInfo(fullName: String, lineNum: Int)
  object PosInfo {
    inline given posInfo(using fullName: sourcecode.FullName.Machine, line: sourcecode.Line): PosInfo =
      PosInfo(fullName = fullName.value, lineNum = line.value)
  }

  enum StreamChecker[T] {
    case Forall(fn: T => Unit, posInfo: PosInfo)
    case Exists(pred: T => Boolean, posInfo: PosInfo)
    case And(left: StreamChecker[T], right: StreamChecker[T])
    case Transform[T, U](fn: T => U, innerChecker: StreamChecker[U]) extends StreamChecker[T]

    def &&(other: StreamChecker[T]): StreamChecker[T] =
      And(this, other)

    def apply(data: Iterator[Example[T]]): Unit = {
      def impl[T](self: StreamChecker[T]): Option[Example[T]] => Unit =
        self match {
          case Forall(fn, posInfo) =>
            {
              case None =>
              case Some(Example(value, _)) =>
                fn(value)
            }
          case Exists(pred, posInfo) =>
            var count = 0
            {
              case None =>
                assert(count > 0, s"no examples found for ${posInfo.fullName}:${posInfo.lineNum}")
              case Some(Example(value, _)) =>
                if(pred(value)) {
                  count += 1
                }
            }
          case And(left, right) =>
            val leftFn = impl(left)
            val rightFn = impl(right)
            { exampleOpt =>
              leftFn(exampleOpt)
              rightFn(exampleOpt)
            }
          case Transform(fn, innerChecker) =>
            val innerFn = impl(innerChecker)
            { exampleOpt =>
              innerFn {
                exampleOpt.map {
                  case Example(value, budget) => Example(fn(value), budget)
                }
              }
            }
        }
      
      val check = impl(this)
      data.map(Some(_)).foreach(check)
      check(None) // check at end
    }
  }

  def forall[T](fn: T => Unit)(using posInfo: PosInfo): StreamChecker[T] =
    StreamChecker.Forall(fn, posInfo)

  def exists[T](pred: T => Boolean)(using posInfo: PosInfo): StreamChecker[T] =
    StreamChecker.Exists(pred, posInfo)

  def transform[T, U](fn: T => U)(innerChecker: StreamChecker[U]): StreamChecker[T] =
    StreamChecker.Transform(fn, innerChecker)

  def apply[T](fn: Generator[T] => Unit): Checker[T] =
    new Checker[T] {
      override def apply(gen: Generator[T]): Unit = fn(gen)
    }

  private def humanDuration(duration: Duration): String = {
    val builder = mutable.StringBuilder.newBuilder
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

  def timeLimited[T](maxDuration: Duration, printRoundExample: Boolean = true)(streamChecker: StreamChecker[T]): Checker[T] =
    Checker { gen =>
      val startTime = Instant.now()

      // state space analytics
      var countExplored = 0
      var countExploredSinceLast = 0
      var lastExample: Option[Example[T]] = None
      var roundStart = Instant.now()

      var roundNum = 0
      try {
        streamChecker {
          Iterator.continually {
            if(roundNum > 0) {
              println(s"explored round ${roundNum - 1}: took ${humanDuration(Duration.between(roundStart, Instant.now()))} and covered $countExplored states so far ($countExploredSinceLast since last msg)")
              countExploredSinceLast = 0
              if(lastExample.nonEmpty && printRoundExample) {
                print("  printing last example from round: ")
                pprint.tokenize(lastExample.get, initialOffset = 2).foreach(print)
                println()
              }
            }
            
            val roundToGen = roundNum
            roundNum += 1
            roundStart = Instant.now()
            gen.computeResultsForRound(round = roundToGen)
          }
          .flatten
          .takeWhile { _ =>
            // don't call now() too often. every 1000 might be a good balance of accuracy and rate reduction
            if(countExplored % 1000 == 0) {
              Instant.now().isBefore(startTime.plus(maxDuration))
            } else {
              true
            }
          }
          .flatten
          .map(Example(_, roundNum))
          .tapEach { example =>
            lastExample = Some(example)
            countExplored += 1
            countExploredSinceLast += 1
          }
        }
          
        assert(countExplored > 0, s"checked no values - that's usually bad")
        println(fansi.Color.Green(">>successful checking") ++ s" after ${humanDuration(Duration.between(startTime, Instant.now()))}, after exploring $countExplored states")
      } catch {
        case err =>
          println(fansi.Color.Red("!!found error") ++ s" after ${humanDuration(Duration.between(startTime, Instant.now()))}, exploring $countExplored states")
          if(lastExample.nonEmpty) {
            print("  printing last example considered: ")
            pprint.tokenize(lastExample, initialOffset = 2).foreach(print)
            println()
          }
          throw err
      }
    }
}
