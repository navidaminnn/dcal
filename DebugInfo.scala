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

  @scala.annotation.targetName("concat")
  def ++(other: DebugInfo): DebugInfo =
    DebugInfo.Multi(this, other)

object DebugInfo:
  given instance(using file: File, line: Line): DebugInfo =
    DebugInfo.Single(file.value, line.value, None)

  inline def apply()(using DebugInfo): DebugInfo = summon[DebugInfo]

  // Put this in implicit scope (as an inline given) when you want to be sure you're not accidentally
  // summoning DebugInfo that points inside your implementation.
  // It will flag all such mistakes with a compile-time error.
  inline def poison: DebugInfo =
    scala.compiletime.error("implementation bug: used a poison DebugInfo value")

  // If you have a nested scope where you used the above but actually want DebugInfo
  // to work again, shadow the poison given with another inline given that expands to this.
  inline def notPoison: DebugInfo =
    import scala.compiletime.summonInline
    instance(using file = summonInline[File], line = summonInline[Line])
