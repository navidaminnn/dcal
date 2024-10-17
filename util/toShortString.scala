package distcompiler.util

extension (obj: Object)
  def toShortString(lineLimit: Int = 10): String =
    val lines = obj.toString().linesIterator
    val trimmed =
      lines.take(lineLimit)
        ++ (if lines.hasNext then Iterator.single("...") else Iterator.empty)
    trimmed.mkString("\n")
