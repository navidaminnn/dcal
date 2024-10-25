//> using dep com.lihaoyi::os-lib:0.11.3
package distcompiler

import scala.util.matching.Regex
import java.time.LocalDate
import java.time.format.DateTimeFormatter

@main
def updateLicense(check: Boolean): Unit =
  val licenseTemplate =
    """ Copyright ____ DCal Team
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
      | limitations under the License."""
      .stripMargin
      .linesIterator
      .map(line => s"//$line")
      .mkString(System.lineSeparator())

  val yearString = LocalDate.now().format(DateTimeFormatter.ofPattern("yyyy"))
  val licenseText = licenseTemplate.replace("____", yearString)
  val licenseRegex = raw"""// Copyright \d\d\d\d.*// limitations under the license.""".r
  val licenseReplacement = Regex.quoteReplacement(licenseText)

  var checkFailed = false

  os.walk(os.pwd)
    .iterator
    .filterNot(_.segments.exists(_.startsWith(".")))
    .filter(_.last.endsWith(".scala"))
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
            println(s"replace $str")
            println(s"with $licenseReplacement")
            licenseRegex.replaceFirstIn(contents, licenseReplacement)

      if check
      then
        if contents != modifiedContents
        then
          checkFailed = true
          println(s"license needs updating in $sourceFile")
      else os.write.over(sourceFile, modifiedContents)

  if check && checkFailed
  then
    println("check failed. TODO: update license info")
    System.exit(1)
