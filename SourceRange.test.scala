// Copyright 2024-2025 DCal Team
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

package distcompiler

import scala.collection.StringOps
import java.nio.charset.StandardCharsets

class SourceRangeTests extends munit.FunSuite:
  val ipsum =
    """Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor
      |incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud
      |exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure
      |dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur.
      |Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit
      |anim id est laborum.""".stripMargin

  extension (src: SourceRange)
    def takeLine: SourceRange =
      src.take(src.indexOf('\n'))
    def dropLine: SourceRange =
      src.drop(src.indexOf('\n') + 1)
    def lastLine: SourceRange =
      src.drop(src.lastIndexOf('\n') + 1)
    def startingFrom(str: String): SourceRange =
      val bytes = str.getBytes()
      src.drop(src.indexOfSlice(bytes))
    def upTo(str: String): SourceRange =
      val bytes = str.getBytes()
      src.take(src.indexOfSlice(bytes) + bytes.size)

  private trait MarginStripper:
    extension (str: String) def stripMargin: String

  extension (str: String)
    def ensureLf: String =
      str.split("\r\n").mkString("\n")
    def ensureCrLf: String =
      str.ensureLf.split("\n").mkString("\r\n")

  test("sanity: crlf and lf normalization"):
    assertEquals("a\nb\nc".ensureLf, "a\nb\nc")
    assertEquals("a\r\nb\r\nc".ensureLf, "a\nb\nc")
    assertEquals("a\nb\nc".ensureCrLf, "a\r\nb\r\nc")
    assertEquals("a\r\nb\r\nc".ensureCrLf, "a\r\nb\r\nc")

  locally:
    given MarginStripper with
      extension (str: String)
        def stripMargin: String =
          StringOps(str).stripMargin.ensureLf
    mkTests("[lf]", ipsum.ensureLf)

  locally:
    given MarginStripper with
      extension (str: String)
        def stripMargin: String =
          StringOps(
            str
          ).stripMargin.ensureLf // output is standardized to use \n only
    mkTests("[crlf]", ipsum.ensureCrLf)

  extension (str: String)
    // This helps debug weird control character issues by making them explicit.
    private def toHexView: String =
      val bytes = str.getBytes(StandardCharsets.UTF_8)
      bytes
        .sliding(8, step = 8)
        .map: octet =>
          val chars = octet
            .map:
              case '\n' => "\\n"
              case '\r' => "\\r"
              case ch   => s" ${ch.toChar}"
            .mkString
          val hexs = octet.map(b => f"$b%02X").mkString(" ")
          s"${chars.padTo(16, ' ')} | ${hexs}"
        .mkString("\n")

  private def mkTests(mode: String, ipsum: String)(using MarginStripper): Unit =
    val ipsumSrc = SourceRange.entire(Source.fromString(ipsum))

    def assertHexEquals(expectedStr: String, actualStr: String)(using
        munit.Location
    ): Unit =
      assertEquals(
        s"$expectedStr\n\n${expectedStr.toHexView}",
        s"$actualStr\n\n${actualStr.toHexView}"
      )

    test(s"highlight entire first line $mode"):
      assertHexEquals(
        ipsumSrc.takeLine.showInSource,
        """Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor
          |^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^""".stripMargin
      )

    test(s"highlight entire second line $mode"):
      assertHexEquals(
        ipsumSrc.dropLine.takeLine.showInSource,
        """incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud
          |^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^""".stripMargin
      )

    test(s"highlight last line $mode"):
      assertHexEquals(
        ipsumSrc.lastLine.showInSource,
        """anim id est laborum.
          |^^^^^^^^^^^^^^^^^^^^""".stripMargin
      )

    test(s"highlight second 2 lines $mode"):
      assertHexEquals(
        (ipsumSrc.dropLine.takeLine <+> ipsumSrc.dropLine.dropLine.takeLine).showInSource,
        """vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
          |incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud
          |exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure
          |^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^""".stripMargin
      )

    test(s"highlight second 3 lines $mode"):
      assertHexEquals(
        (ipsumSrc.dropLine.takeLine <+> ipsumSrc.dropLine.dropLine.dropLine.takeLine).showInSource,
        """vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
          |incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud
          |...
          |dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur.
          |^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^""".stripMargin
      )

    test(s"highlight all but 1st line $mode"):
      assertHexEquals(
        ipsumSrc.dropLine.showInSource,
        """vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
          |incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud
          |...
          |anim id est laborum.
          |^^^^^^^^^^^^^^^^^^^^""".stripMargin
      )

    test(s"highlight amet to nisi $mode"):
      assertHexEquals(
        ipsumSrc.startingFrom("amet").upTo("nisi").showInSource,
        """                      vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
          |Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor
          |...
          |exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure
          |^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^""".stripMargin
      )

    test(s"highlight just nisi $mode"):
      assertHexEquals(
        ipsumSrc.startingFrom("nisi").take("nisi".size).showInSource,
        """exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure
          |                             ^^^^""".stripMargin
      )
  end mkTests
