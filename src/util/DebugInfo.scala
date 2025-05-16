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

package forja

import sourcecode.*

final case class DebugInfo(
    file: String,
    fileName: String,
    line: Int,
    outerOpt: Option[DebugInfo] = None
) extends DebugInfo.Ctx:
  override def toString(): String =
    outerOpt match
      case None =>
        s"$file:$line"
      case Some(outer) =>
        s"$file:$line in $outer"

object DebugInfo:
  import scala.compiletime.{summonInline, summonFrom}

  sealed abstract class Ctx

  inline given instance(using
      file: File,
      fileName: FileName,
      line: Line
  ): DebugInfo =
    summonFrom:
      case ctx: Ctx =>
        ctx match
          case ctx: DebugInfo
              if (file.value, fileName.value, line.value) != (
                ctx.file,
                ctx.fileName,
                ctx.line
              ) =>
            DebugInfo(file.value, fileName.value, line.value, Some(ctx))
          case ctx: DebugInfo => ctx
      case _ =>
        DebugInfo(file.value, fileName.value, line.value)

  inline def apply()(using debugInfo: DebugInfo): DebugInfo = debugInfo

  // Put this in implicit scope (as an inline given) when you want to be sure you're not accidentally
  // summoning DebugInfo.
  // It will flag all such mistakes with a compile-time error.
  inline def poison: DebugInfo =
    scala.compiletime.error("implementation bug: used a poison DebugInfo value")

  // If you have a nested scope where you used the above but actually want DebugInfo
  // to work again, shadow the poison given with another inline given that expands to this.
  inline def notPoison: DebugInfo =
    instance(using
      file = summonInline[File],
      fileName = summonInline[FileName],
      line = summonInline[Line]
    )
