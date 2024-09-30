package distcompiler.sexpr

import cats.syntax.all.given

import scala.collection.mutable

import distcompiler.*

object tokens:
  object List extends Token
  object Atom extends Token:
    override def showSource: Boolean = true
  object String extends Token:
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
    val top = reader(sourceRange)
    passes.perform(top)
    top

  import dsl.*

  private lazy val passes: Manip[Unit] =
    pass(once = true)
      .rules:
        on(
          tok(tokens.String).map(_.sourceRange)
        ).rewrite: str =>
          val locBuilder = SourceRange.newBuilder
          val nodeBuffer = mutable.ListBuffer.empty[Node]

          def impl(str: SourceRange): Unit =
            str match
              case src"\\\\$rest" =>
                locBuilder.addOne('\\')
                impl(rest)
              case src"\\\"$rest" =>
                locBuilder.addOne('"')
                impl(rest)
              case src"\\'$rest" =>
                locBuilder.addOne('\'')
                impl(rest)
              case src"\\n$rest" =>
                locBuilder.addOne('\n')
                impl(rest)
              case src"\\t$rest" =>
                locBuilder.addOne('\t')
                impl(rest)
              case src"\\r$rest" =>
                locBuilder.addOne('\r')
                impl(rest)
              case src"\\\n\r$rest" => impl(rest)
              case src"\\\r\n$rest" => impl(rest)
              case src"\\\n$rest"   => impl(rest)
              case src"\\\r$rest"   => impl(rest)
              case src"\\$rest" =>
                nodeBuffer += Builtin.Error(
                  "invalid string escape",
                  Builtin.SourceMarker().at(rest.take(1))
                )
                locBuilder.addOne('?')
                impl(rest.drop(1))
              case _ if str.nonEmpty =>
                locBuilder.addOne(str.head)
                impl(str.tail)
              case _ => ()

          impl(str)

          nodeBuffer += tokens.Atom(locBuilder.result())
          Splice(nodeBuffer.result()*)
