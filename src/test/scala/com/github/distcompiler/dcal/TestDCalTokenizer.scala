package com.github.distcompiler.dcal

import org.scalatest.funsuite.AnyFunSuite

class TestDCalTokenizer extends AnyFunSuite {
  import DCalTokenizer.*

  test("annotateCharactersWithPosition simple cases") {
    def runWithDefaults(str: String): List[(Char,Position)] =
      annotateCharactersWithPosition(
        chars = str.view,
        fileName = "<testfile>",
      )
        .toList

    def pos(line: Int, column: Int): Position =
      Position(fileName = "<testfile>", line = line, column = column)

    assert(runWithDefaults("") == Nil)
    assert(runWithDefaults("abc") == List(
      ('a', pos(1, 1)),
      ('b', pos(1, 2)),
      ('c', pos(1, 3)),
    ))
    assert(runWithDefaults("a\nb") == List(
      ('a', pos(1, 1)),
      ('\n', pos(2, 0)),
      ('b', pos(2, 1)),
    ))
    assert(runWithDefaults("a\nb\ncd") == List(
      ('a', pos(1, 1)),
      ('\n', pos(2, 0)),
      ('b', pos(2, 1)),
      ('\n', pos(3, 0)),
      ('c', pos(3, 1)),
      ('d', pos(3, 2)),
    ))
  }
}
