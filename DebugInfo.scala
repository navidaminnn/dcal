package distcompiler

import sourcecode.*

enum DebugInfo:
  case Single(file: String, line: Int, by: Option[DebugInfo])
  case Multi(left: DebugInfo, right: DebugInfo)

  override def toString(): String =
    def getSingles(info: DebugInfo): Iterator[DebugInfo.Single] =
      info match
        case single: DebugInfo.Single => Iterator.single(single)
        case Multi(left, right) =>
          getSingles(left) ++ getSingles(right)

    def singleToString(single: DebugInfo.Single): String =
      s"${single.file}:${single.line}${single.by match
          case None       => ""
          case Some(info) => " by " ++ info.toString()
        }"

    getSingles(this).map(singleToString).mkString("\nand ")

  def ++(other: DebugInfo): DebugInfo =
    DebugInfo.Multi(this, other)

object DebugInfo:
  given instance(using file: File, line: Line): DebugInfo =
    DebugInfo.Single(file.value, line.value, None)

  inline def apply()(using DebugInfo): DebugInfo = summon[DebugInfo]
