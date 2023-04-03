package com.github.distcompiler.dcal

object TestUtils {
  def sequenceLines(lines: String*): String = lines.mkString("\n")
  def readTestFile(fileName: String): String =
    val specPath = os.pwd / "src" / "test" / "resources" / "DCal" / fileName
    os.read(specPath)
}
