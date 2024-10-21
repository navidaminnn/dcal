package distcompiler.util

import scala.util.control.TailCalls.*

object TailCallsUtils:
  extension [T](iter: Iterator[T])
    def intercalate(value: T): Iterator[T] =
      new Iterator[T]:
        var prependSep = false
        def hasNext: Boolean = iter.hasNext
        def next(): T =
          if prependSep && iter.hasNext
          then
            prependSep = false
            value
          else
            prependSep = true
            iter.next()

    def traverse(fn: T => TailRec[Unit]): TailRec[Unit] =
      def impl: TailRec[Unit] =
        if !iter.hasNext
        then done(())
        else
          for
            () <- tailcall(fn(iter.next()))
            () <- tailcall(impl)
          yield ()

      impl
