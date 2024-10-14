package distcompiler.sexpr

import cats.syntax.all.given

import scala.collection.mutable

import distcompiler.*

object tokens:
  object List extends Token
  object Atom extends Token:
    override def showSource: Boolean = true

val wellFormed: Wellformed =
  import distcompiler.wf.*
  Wellformed:
    val listContents = repeated(tok(tokens.Atom, tokens.List))
    Node.Top ::= listContents
    tokens.Atom ::= Atom
    tokens.List ::= listContents

object parse:
  def fromFile(path: os.Path): Node.Top =
    fromSourceRange:
      SourceRange.entire(Source.mapFromFile(path))

  def fromSourceRange(sourceRange: SourceRange): Node.Top =
    SExprReader(sourceRange)

object serialize:
  // using TailCalls rather than cats.Eval because we are mixing imperative
  // and lazy code, and I ran into a bug where cats.Eval (reasonably for its normal use but not here)
  // silently memoized an effectful computation
  import scala.util.control.TailCalls.*

  extension [T](iter: Iterator[T])
    private def intercalate(value: T): Iterator[T] =
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
    private def traverse(fn: T => TailRec[Unit]): TailRec[Unit] =
      def impl: TailRec[Unit] =
        if !iter.hasNext
        then done(())
        else
          for
            () <- fn(iter.next())
            () <- impl
          yield ()

      impl

  def toPrettyString(top: Node.All): String =
    val out = java.io.ByteArrayOutputStream()
    toPrettyWritable(top).writeBytesTo(out)
    out.toString()

  def toCompactWritable(top: Node.All): geny.Writable =
    new geny.Writable:
      override def writeBytesTo(out: java.io.OutputStream): Unit =
        def impl(node: Node.All): TailRec[Unit] =
          (node: @unchecked) match
            case top: Node.Top =>
              top.children.iterator
                .map(impl)
                .traverse(identity)
            case atom @ tokens.Atom() =>
              val sourceRange = atom.sourceRange
              out.write(sourceRange.length.toString().getBytes())
              out.write(':')
              sourceRange.writeBytesTo(out)
              done(())
            case list @ tokens.List(_*) =>
              for
                () <- done(out.write('('))
                () <- list.children.iterator
                  .traverse(impl)
                () <- done(out.write(')'))
              yield ()

        impl(top).result

  def toPrettyWritable(top: Node.All): geny.Writable =
    new geny.Writable:
      override def writeBytesTo(out: java.io.OutputStream): Unit =
        var indentLevel = 0

        def lzy[T](fn: => T): TailRec[T] =
          tailcall(done(fn))

        val nl: TailRec[Unit] =
          lzy:
            out.write('\n')
            (0 until indentLevel).foreach(_ => out.write(' '))

        def withIndent(fn: => TailRec[Unit]): TailRec[Unit] =
          indentLevel += 2
          for
            () <- tailcall(fn)
            () <- done(indentLevel -= 2)
          yield ()

        def impl(node: Node.All): TailRec[Unit] =
          (node: @unchecked) match
            case top: Node.Top =>
              top.children.iterator
                .map(impl)
                .intercalate(nl)
                .traverse(identity)
            case atom @ tokens.Atom() =>
              val sourceRange = atom.sourceRange
              out.write(sourceRange.length.toString().getBytes())
              out.write(':')
              sourceRange.writeBytesTo(out)
              done(())
            case tokens.List() =>
              out.write('(')
              out.write(')')
              done(())
            case tokens.List(child) =>
              for
                () <- done(out.write('('))
                () <- impl(child)
                () <- done(out.write(')'))
              yield ()
            case list @ tokens.List(_*) =>
              def writeChildren(iter: Iterator[Node.Child]): TailRec[Unit] =
                iter
                  .map(impl)
                  .intercalate(nl)
                  .traverse(identity)

              out.write('(')
              for
                () <- withIndent:
                  list.children.head match
                    case atom @ tokens.Atom() =>
                      for
                        () <- impl(atom)
                        () <- nl
                        () <- writeChildren(list.children.iterator.drop(1))
                      yield ()
                    case _ =>
                      for
                        () <- nl
                        () <- writeChildren(list.children.iterator)
                      yield ()
                () <- done(out.write(')'))
              yield ()

        impl(top).result
