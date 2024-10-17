package distcompiler.tla

import distcompiler.*

class TLAParserTests extends munit.FunSuite, test.WithTLACorpus:
  self =>

  testWithCorpusFile: file =>
    val src = Source.mapFromFile(file)
    val top = TLAReader(SourceRange.entire(src))

    assume(!top.hasErrors, top)

    TLAParser(
      top /*, tracer = Manip.RewriteDebugTracer(os.pwd / "parser_passes")*/
    )

    os.write.over(
      os.pwd / "dbg_tla_parser" / s"${file.last}.dbg",
      top.toPrettyWritable(TLAReader.wellformed),
      createFolders = true
    )

    if top.hasErrors
    then fail(top.presentErrors())

    if top.children.isEmpty
    then fail("no data extracted")
