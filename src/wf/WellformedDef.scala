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

import scala.collection.mutable
import forja.dsl.*

trait WellformedDef:
  val topShape: Shape

  trait t protected (private[WellformedDef] val shape: Shape) extends Token

  protected def forceDef(tok: t): Unit =
    tok ::= tok.shape

  protected given builder: Wellformed.Builder =
    Wellformed.Builder(mutable.HashMap.empty, None)

  final lazy val wf: Wellformed =
    Node.Top ::= topShape

    val visited = mutable.HashSet[Token]()

    def implTok(tok: Token): Unit =
      if !visited(tok)
      then
        visited += tok
        tok match
          case tok: t =>
            tok ::= tok.shape
            implShape(tok.shape)
          case tok =>
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
  end wf
end WellformedDef
