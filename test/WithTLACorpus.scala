package distcompiler.test

transparent trait WithTLACorpus extends WithClonedCorpus:
  self: munit.FunSuite =>
  val repoURL: String = "https://github.com/tlaplus/Examples.git"
  val intoName: String = "Examples"

  def isCorpusFile(file: os.Path): Boolean = file.last.endsWith(".tla")
