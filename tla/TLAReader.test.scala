package distcompiler.tla

import distcompiler.*

class TLAReaderTests extends munit.FunSuite:
  val repoURL: String = "https://github.com/tlaplus/Examples.git"
  val clonesDir = os.pwd / ".clones"
  val intoName = clonesDir / "Examples"

  if !os.exists(intoName)
  then
    os.makeDir.all(clonesDir)
    os.proc("git", "clone", repoURL).call(cwd = os.pwd / ".clones")

  os.walk
    .stream(intoName)
    .filter(_.last.endsWith(".tla"))
    .foreach: exampleFile =>
      test(exampleFile.relativeTo(clonesDir).toString):
        val src = Source.mapFromFile(exampleFile)
        val top = TLAReader(SourceRange.entire(src))

        os.write.over(
          os.pwd / "dbg" / s"${exampleFile.last}.dbg",
          top.toPrettyWritable(TLAReader.wellformed),
          createFolders = true
        )

        if top.hasErrors
        then
          val bigMsg =
            top.errors
              .map: err =>
                val msg = err(Builtin.Error.Message)
                val ast = err(Builtin.Error.AST)

                s"${msg.sourceRange.decodeString()} at ${ast.sourceRange.presentationStringLong}"
              .mkString("\n")

          fail(bigMsg)

        if top.children.isEmpty
        then fail("no data extracted")
