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

import forja.*
import forja.sexpr.lang.{SAtom, SList}

enum Shape:
  case Atom, AnyShape
  case Choice(choices: Set[Token | EmbedMeta[?]])
  case Repeat(choice: Shape.Choice, minCount: Int)
  case Fields(fields: List[Shape.Choice])

  def asNode: Node =
    this match
      case Atom     => SAtom("atom")
      case AnyShape => SAtom("anyShape")
      case Choice(choices) if choices.sizeIs == 1 =>
        choices.head match
          case token: Token        => SAtom(token.name)
          case embed: EmbedMeta[?] => SAtom(embed.canonicalName)
      case Choice(choices) =>
        val result = SList(
          SAtom("choice"),
        )
        result.children.addAll:
          choices.iterator
            .map:
              case token: Token => SAtom(token.name)
              case embed: EmbedMeta[?] =>
                SAtom(embed.canonicalName)
        result
      case Repeat(choice, minCount) =>
        SList(
          SAtom("repeat"),
          choice.asNode,
          SList(
            SAtom("minCount"),
            SAtom(minCount.toString()),
          ),
        )
      case Fields(fields) =>
        val result = SList(
          SAtom("fields"),
        )
        result.children.addAll(fields.iterator.map(_.asNode))
        result

object Shape:
  extension (choice: Shape.Choice)
    @scala.annotation.targetName("combineChoices")
    def |(other: Shape.Choice): Shape.Choice =
      Shape.Choice(choice.choices ++ other.choices)

  trait Ops:
    def choice(tokens: (Token | EmbedMeta[?])*): Shape.Choice =
      Shape.Choice(tokens.toSet)

    def choice(tokens: Set[Token | EmbedMeta[?]]): Shape.Choice =
      Shape.Choice(tokens)

    def repeated(choice: Shape.Choice, minCount: Int = 0): Shape.Repeat =
      Shape.Repeat(choice, minCount)

    inline def embedded[T: EmbedMeta]: Shape.Choice =
      Shape.Choice(Set(summon[EmbedMeta[T]]))

    def fields(fields: Shape.Choice*): Shape.Fields =
      Shape.Fields(fields.toList)

    import scala.language.implicitConversions
    // TODO: once `into` keyword works, use that
    implicit def tokenAsChoice(token: Token): Shape.Choice =
      Shape.Choice(Set(token))
  end Ops
end Shape
