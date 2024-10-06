package distcompiler.util

final case class UnreachableError()
    extends Error("this code should not be reachable")

@scala.annotation.targetName("unreachable")
def !!! : Nothing =
  throw UnreachableError()
