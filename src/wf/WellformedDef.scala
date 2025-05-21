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

package forja.wf

import forja.dsl.*
import forja.util.Named

import scala.collection.mutable
import scala.util.control.NonFatal

trait WellformedDef:
  def topShape: Shape

  abstract class t protected (shapeFn: => Shape)(using Named.OwnName)
      extends Token:
    private[WellformedDef] lazy val shape = shapeFn

  protected def forceDef(tok: t): Unit =
    tok ::= tok.shape

  protected given builder: Wellformed.Builder =
    Wellformed.Builder(mutable.HashMap.empty, None)

  final lazy val wf: Wellformed =
    val visited = mutable.HashSet[Token]()
    // If .wf crashes (throw during lazy val init), make a best effort to reset
    /* mutable state. That is, if you see it crash repeatedly, all the stack
     * traces */
    // show the same (hopefully actual) problem, not just the effect of
    // running part of this procedure twice, which will almost definitely
    // crash due to redefinitions.
    try
      Node.Top ::= topShape

      def implTok(tok: Token): Unit =
        if !visited(tok)
        then
          tok match
            case tok: t =>
              tok ::= tok.shape
              // add after ::= for exception safety (see catch below)
              visited += tok
              implShape(tok.shape)
            case tok =>
              visited += tok
              implShape(tok.existingShape)

      def implShape(shape: Shape): Unit =
        shape match
          case Shape.Atom     =>
          case Shape.AnyShape =>
          case Shape.Choice(choices) =>
            choices.foreach:
              case tok: Token => implTok(tok)
              case _          =>
          case Shape.Repeat(choice, minCount) =>
            implShape(choice)
          case Shape.Fields(fields) =>
            fields.foreach(implShape)

      implShape(topShape)

      builder.build()
    catch
      case NonFatal(ex) =>
        Node.Top.undef()
        visited.foreach:
          case tok: t =>
            tok.undef()
          case _ => // skip
        throw ex
  end wf
end WellformedDef
