package distcompiler.test

transparent trait WithClonedCorpus:
  self: munit.FunSuite =>
  def repoURL: String
  def intoName: String

  def isCorpusFile(file: os.Path): Boolean

  def clonesDir: os.Path = os.pwd / ".clones"
  private def checkoutFolder = clonesDir / intoName

  def testWithCorpusFile(using munit.Location)(fn: os.Path => Unit): Unit =
    if !os.exists(checkoutFolder)
    then
      os.makeDir.all(clonesDir)
      os.proc("git", "clone", repoURL, checkoutFolder).call()

    var foundAnything = false

    os.walk
      .stream(checkoutFolder)
      .filter(isCorpusFile)
      .foreach: corpusFile =>
        foundAnything = true
        test(corpusFile.relativeTo(clonesDir).toString)(fn(corpusFile))

    test("sanity"):
      assert(
        foundAnything,
        s"must find at least one test file in $checkoutFolder"
      )
