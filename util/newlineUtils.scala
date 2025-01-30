package distcompiler.util

object newlineUtils:
  extension (str: String)
    def ensureLf: String =
      str.split("\r\n").mkString("\n")
    def ensureCrLf: String =
      str.ensureLf.split("\n").mkString("\r\n")
