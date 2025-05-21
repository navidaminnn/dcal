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

package scripts

import java.time.LocalDate
import java.time.format.DateTimeFormatter

import scala.util.matching.Regex

@main
def update_license_sc(args: String*): Unit =
  var shouldRewrite = false

  val parser = new scopt.OptionParser[Unit]("update_license"):
    cmd("rewrite")
      .optional()
      .foreach(_ => shouldRewrite = true)
      .text("rewrite the source files")
    cmd("dry-run")
      .optional()
      .foreach(_ => shouldRewrite = false)
      .text("don't touch any of the files")

  if !parser.parse(args, ()).isDefined
  then sys.exit(1)

  val licenseTemplate =
    """ Copyright 2024-____ Forja Team
      |
      | Licensed under the Apache License, Version 2.0 (the "License");
      | you may not use this file except in compliance with the License.
      | You may obtain a copy of the License at
      |
      |     http://www.apache.org/licenses/LICENSE-2.0
      |
      | Unless required by applicable law or agreed to in writing, software
      | distributed under the License is distributed on an "AS IS" BASIS,
      | WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
      | See the License for the specific language governing permissions and
      | limitations under the License.""".stripMargin.linesIterator
      .map(line => s"//$line")
      .mkString(System.lineSeparator())

  val yearString = LocalDate.now().format(DateTimeFormatter.ofPattern("yyyy"))
  val licenseText = licenseTemplate.replace("____", yearString)
  val licenseRegex = Regex(
    raw"""(?s)// Copyright \d\d\d\d.*// limitations under the License\.""",
  )
  val licenseReplacement = Regex.quoteReplacement(licenseText)

  var checkFailed = false

  os.walk(os.pwd)
    .iterator
    .filterNot(_.segments.exists(_.startsWith(".")))
    .filterNot(_ == os.pwd / "project.scala") // conflict with fix rule
    .filter(p => p.last.endsWith(".scala") || p.last.endsWith(".sc"))
    .foreach: sourceFile =>
      val contents = os.read(sourceFile)

      val modifiedContents =
        licenseRegex.findFirstIn(contents) match
          case None =>
            licenseText
              ++ System.lineSeparator()
              ++ System.lineSeparator()
              ++ contents
          case Some(str) =>
            licenseRegex.replaceFirstIn(contents, licenseReplacement)

      if shouldRewrite
      then os.write.over(sourceFile, modifiedContents)
      else if contents != modifiedContents
      then
        checkFailed = true
        println(s"license needs updating in $sourceFile")

  if shouldRewrite
  then println("all changes made.")
  else if checkFailed
  then
    locally {
      /* Mystery: without locally { ... }, compiler rejects this.
       * You can make it work with more indentation, but format changes it back
       * to the inexplicably broken version. */
      println("check failed. TODO: update license info")
      System.exit(1)
    }
  else println("check ok, all licenses up to date.")
end update_license_sc
