package distcompiler.util

final case class UnreachableError()
    extends Error("this code should not be reachable")

def !!! : Nothing =
  throw UnreachableError()
