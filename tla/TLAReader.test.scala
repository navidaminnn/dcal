package distcompiler.tla

import distcompiler.*

class TLAReaderTests extends munit.FunSuite, test.WithTLACorpus:
  self => // funny parser ambiguity: try deleting this line

  testWithCorpusFile: file =>
    val src = Source.mapFromFile(file)
    val top = TLAReader(SourceRange.entire(src))

    os.write.over(
      os.pwd / "dbg_tla_reader" / s"${file.last}.dbg",
      top.toPrettyWritable(TLAReader.wellformed),
      createFolders = true
    )

    if top.hasErrors
    then fail(top.presentErrors(debug = true))

    if top.children.isEmpty
    then fail("no data extracted")
