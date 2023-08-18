import mill._, scalalib._

object dcal extends RootModule with ScalaModule {
  def scalaVersion = "3.3.0"
  def scalacOptions = Seq(
    "-source", "future",
    "-feature",
    "-deprecation",
    "-Werror",
    "-language:strictEquality",
  )

  def ivyDeps = Agg(
    ivy"com.lihaoyi::os-lib:0.9.1",
    ivy"com.lihaoyi::sourcecode:0.3.0",
    ivy"com.lihaoyi::pprint:0.8.1",
    ivy"com.lihaoyi::fansi:0.4.0",
    ivy"org.typelevel::cats-core:2.9.0",
    ivy"org.typelevel::alleycats-core:2.9.0",
    ivy"org.typelevel::kittens:3.0.0",
  )

  object test extends ScalaTests with TestModule.Munit {
    def ivyDeps = Agg(ivy"org.scalameta::munit:1.0.0-M8")
  }
}
