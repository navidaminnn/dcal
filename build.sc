import mill._, scalalib._

object dcal extends RootModule with ScalaModule {
  def scalaVersion = "3.3.0"

  def ivyDeps = Agg(
    ivy"com.lihaoyi::os-lib:0.9.1",
    ivy"org.typelevel::cats-core:2.9.0",
  )

  object test extends ScalaTests {
    def ivyDeps = Agg(ivy"com.lihaoyi::utest:0.8.1")
    def testFramework = "utest.runner.Framework"
  }
}
