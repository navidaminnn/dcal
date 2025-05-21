// Copyright 2024-2025 Forja Team
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

package forja.sexpr

import cats.syntax.all.given

import forja.*
import forja.dsl.*
import forja.src.{Source, SourceRange}
import forja.wf.WellformedDef

object lang extends WellformedDef:
  lazy val topShape: Shape = repeated(choice(SAtom, SList))
  object SList extends t(topShape)
  object SAtom extends t(Atom), Token.ShowSource

object parse:
  def fromFile(path: os.Path): Node.Top =
    fromSourceRange:
      SourceRange.entire(Source.mapFromFile(path))

  def fromSourceRange(sourceRange: SourceRange): Node.Top =
    SExprReader(sourceRange)

object serialize:
  /* Using TailCalls here rather than cats.Eval because we are mixing imperative
   * and lazy code, and I ran into a bug where cats.Eval (reasonably for its
   * normal use but not here) silently memoized an effectful computation. */
  import scala.util.control.TailCalls.*
  import forja.util.TailCallsUtils.*

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
            case atom @ lang.SAtom() =>
              val sourceRange = atom.sourceRange
              out.write(sourceRange.length.toString().getBytes())
              out.write(':')
              sourceRange.writeBytesTo(out)
              done(())
            case list @ lang.SList(_*) =>
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
            case atom @ lang.SAtom()
                if SExprReader.canBeEncodedAsToken(atom.sourceRange) =>
              atom.sourceRange.writeBytesTo(out)
              done(())
            case atom @ lang.SAtom() =>
              val sourceRange = atom.sourceRange
              out.write(sourceRange.length.toString().getBytes())
              out.write(':')
              sourceRange.writeBytesTo(out)
              done(())
            case lang.SList() =>
              out.write('(')
              out.write(')')
              done(())
            case lang.SList(child) =>
              for
                () <- done(out.write('('))
                () <- impl(child)
                () <- done(out.write(')'))
              yield ()
            case list @ lang.SList(_*) =>
              def writeChildren(iter: Iterator[Node.Child]): TailRec[Unit] =
                iter
                  .map(impl)
                  .intercalate(nl)
                  .traverse(identity)

              out.write('(')
              for
                () <- withIndent:
                  list.children.head match
                    case atom @ lang.SAtom() =>
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
