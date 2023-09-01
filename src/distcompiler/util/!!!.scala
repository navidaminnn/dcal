package distcompiler.util

@scala.annotation.targetName("unreachable")
inline def !!! : Nothing =
  throw Unreachable()

final class Unreachable extends Exception("unreachable")
