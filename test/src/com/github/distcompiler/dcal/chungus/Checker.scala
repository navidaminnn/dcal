package test.com.github.distcompiler.dcal.chungus

import java.time.{Instant, Duration}
import scala.concurrent.duration.fromNow
import scala.collection.mutable
import scala.annotation.targetName

sealed trait Checker[T] { self =>
  def apply(gen: Generator[T]): Unit
}

object Checker {
  final case class Example[T](value: T, budget: Int)

  sealed trait StreamChecker[T] { self =>
    def apply(data: Iterator[Example[T]]): Iterator[Example[T]]

    @targetName("and")
    def &&(other: StreamChecker[T]): StreamChecker[T] =
      StreamChecker { data =>
        other(self(data))
      }
  }
  object StreamChecker {
    def apply[T](fn: Iterator[Example[T]] => Iterator[Example[T]]): StreamChecker[T] =
      new StreamChecker[T] {
        override def apply(data: Iterator[Example[T]]): Iterator[Example[T]] = fn(data)
      }

    // a tricky way to get an iterator that runs code when consumed but never yields anything
    private def iterSentinel(fn: =>Unit): Iterator[Nothing] =
      Iterator.unfold(None) { _ =>
        fn
        None
      }

    def exists[T](pred: T => Boolean)(using fullName: sourcecode.FullName, lineNum: sourcecode.Line): StreamChecker[T] =
      StreamChecker { data =>
        var count = 0
        data.tapEach {
          case Example(value, _) if pred(value) =>
            count += 1
          case _ =>
        } ++ iterSentinel {
          assert(count > 0, s"no examples found for ${fullName.value}:${lineNum.value}")
        }
      }

    def forall[T](fn: T => Unit): StreamChecker[T] =
      StreamChecker { data =>
        data.tapEach {
          case Example(value, _) =>
            fn(value)
        }
      }
  }

  export StreamChecker.{apply as _, *}

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

    builder.result()
  }

  def timeLimited[T](maxDuration: Duration)(streamChecker: StreamChecker[T]): Checker[T] =
    Checker { gen =>
      val startTime = Instant.now()

      // state space analytics
      var maxCountExplored = 0
      var maxCountExploredCurr = 0
      def maxCountExploredOverall: Int =
        math.max(maxCountExplored, maxCountExploredCurr)

      var lastExample: Option[Example[T]] = None

      var roundStart = Instant.now()
      var budget = 0
      try {
        val count =
          streamChecker {
            Iterator.continually {
              if(budget > 0) {
                println(s"round over. took ${humanDuration(Duration.between(roundStart, Instant.now()))} and covered $maxCountExploredCurr states")
                if(lastExample.nonEmpty) {
                  println("printing last example from round:")
                  pprint.pprintln(lastExample.get)
                }
              }
              maxCountExplored = maxCountExploredCurr
              maxCountExploredCurr = 0

              budget += 1
              println(s"starting round with budget $budget...")
              roundStart = Instant.now()
              gen.apply(budget)
            }
            .flatten
            .takeWhile { _ =>
              // don't call now() too often. every 100 might be a good balance of accuracy and rate reduction
              if(maxCountExploredCurr % 100 == 0) {
                Instant.now().isBefore(roundStart.plus(maxDuration))
              } else {
                true
              }
            }
            .map(_.value)
            .map(Example(_, budget))
            .tapEach { example =>
              lastExample = Some(example)
              maxCountExploredCurr += 1
            }
          }
          .size
          
        assert(count > 0, s"checked no values - that's usually bad")
        println(s"successful checking after ${humanDuration(Duration.between(startTime, Instant.now()))}, after exploring a maximum space of $maxCountExploredOverall states")
      } catch {
        case err =>
          println(s"found error after ${humanDuration(Duration.between(startTime, Instant.now()))}, exploring a maximum space of $maxCountExploredOverall states")
          if(lastExample.nonEmpty) {
            println("printing last example considered:")
            pprint.pprintln(lastExample.get)
          }
          throw err
      }
    }
}
