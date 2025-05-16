// Copyright 2024-2025 Forja Team
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

package forja.util

import scala.quoted.Expr
import scala.quoted.Quotes
import scala.quoted.Type
import edu.berkeley.cs.jqf.fuzz.ei.ZestGuidance
import java.util.Random
import edu.berkeley.cs.jqf.fuzz.junit.GuidedFuzzing
import scala.jdk.CollectionConverters.given
import scala.concurrent.duration.Duration

trait FuzzTestSuite extends munit.FunSuite:
  override def munitTimeout: Duration = Duration("5m")

  def trialLimit: Option[Long] = None
  def fuzzTimeout: Duration = Duration("10s")

  protected inline def fuzzTestMethod(inline method: Any)(using
      loc: munit.Location,
  ): Unit =
    ${ FuzzTestSuite.fuzzTestMethodImpl('this, 'method, 'loc) }

  protected final def doFuzzTest(className: String, methodName: String)(using
      munit.Location,
  ): Unit =
    val buildProc = os.proc(
      "scala-cli",
      "compile",
      "--test",
      ".",
    )
    // println(s"$$ ${buildProc.commandChunks.mkString(" ")}")
    buildProc.call(cwd = os.pwd, mergeErrIntoOut = true)

    val ownClasspath = System.getProperty("java.class.path").split(":")
    val instrumentJar = ownClasspath.find(_.contains("jqf-instrument")).get
    val asmJar = ownClasspath.find(_.contains("/asm")).get
    val classPathWithoutInstrument = ownClasspath
      .filterNot(_.contains("jqf-instrument"))
      .filterNot(_.contains("/asm"))
    val runProc = os.proc(
      "java",
      s"-Xbootclasspath/a:$instrumentJar:$asmJar",
      s"-javaagent:$instrumentJar",
      // s"-Djanala.conf=${os.pwd / "janala.conf"}",
      "-cp",
      classPathWithoutInstrument.mkString(":"),
      FuzzTestSuiteMain.getClass().getCanonicalName().stripSuffix("$"),
      className,
      methodName,
      "--timeout",
      fuzzTimeout.toMillis,
      trialLimit.fold[List[os.Shellable]](Nil)(limit =>
        List("--trial-limit", limit),
      ),
    )
    // println(s"$$ ${runProc.commandChunks.mkString(" ")}")
    val result = runProc.call(
      cwd = os.pwd,
      check = false,
      mergeErrIntoOut = true,
      // stdin = os.Inherit,
      // stdout = os.Inherit,
      // stderr = os.Inherit,
    )
    if result.exitCode != 0
    then fail(result.toString())
end FuzzTestSuite

object FuzzTestSuite:
  def fuzzTestMethodImpl[Self <: FuzzTestSuite: Type](
      selfRef: Expr[Self],
      methodRef: Expr[Any],
      sourceLoc: Expr[munit.Location],
  )(using q: Quotes): Expr[Unit] =
    import q.reflect.*

    val methodCallTerm = methodRef match
      case '{ (arg1: t1) => $fn(arg1): Unit } =>
        Expr.betaReduce('{ $fn(???) }).asTerm
      case '{ (arg1: t1, arg2: t2) => $fn(arg1, arg2): Unit } =>
        Expr.betaReduce('{ $fn(???, ???) }).asTerm
      case '{ (arg1: t1, arg2: t2) => $fn(arg1, arg2): Unit } =>
        Expr.betaReduce('{ $fn(???, ???) }).asTerm
      case '{ (arg1: t1, arg2: t2, arg3: t3) => $fn(arg1, arg2, arg3): Unit } =>
        Expr.betaReduce('{ $fn(???, ???, ???) }).asTerm
      case '{ (arg1: t1, arg2: t2, arg3: t3, arg4: t4) =>
            $fn(arg1, arg2, arg3, arg4): Unit
          } =>
        Expr.betaReduce('{ $fn(???, ???, ???, ???) }).asTerm
      case '{ (arg1: t1, arg2: t2, arg3: t3, arg4: t4, arg5: t5) =>
            $fn(arg1, arg2, arg3, arg4, arg5): Unit
          } =>
        Expr.betaReduce('{ $fn(???, ???, ???, ???, ???) }).asTerm
      case '{ (arg1: t1, arg2: t2, arg3: t3, arg4: t4, arg5: t5, arg6: t6) =>
            $fn(arg1, arg2, arg3, arg4, arg5, arg6): Unit
          } =>
        Expr.betaReduce('{ $fn(???, ???, ???, ???, ???, ???) }).asTerm
      case _ =>
        report.errorAndAbort(
          s"could not identify lambda ${methodRef.show} (note: we only support up to arity 6)",
        )

    object StripIrrelevant:
      def unapply(term: Term): Some[Term] =
        term match
          case Inlined(_, _, StripIrrelevant(term)) => Some(term)
          case Block(_, StripIrrelevant(term))      => Some(term)
          case term                                 => Some(term)

    methodCallTerm match
      case StripIrrelevant(Apply(Select(classExpr, methodName), _)) =>
        classExpr.tpe.classSymbol match
          case None =>
            report.errorAndAbort(
              s"can't get the class type of ${classExpr.show}",
            )
          case Some(classSym) =>
            val className = classSym.fullName
            '{
              given munit.Location = $sourceLoc
              $selfRef.test(${ Expr(s"fuzzTest $className#$methodName") }):
                $selfRef.doFuzzTest(${ Expr(className) }, ${ Expr(methodName) })
            }
      case _ =>
        report.errorAndAbort(
          s"could not find method call in ${methodCallTerm.show}",
        )
  end fuzzTestMethodImpl
end FuzzTestSuite

object FuzzTestSuiteMain:
  def main(argv: Array[String]): Unit =
    var className: String = ""
    var methodName: String = ""
    var trialLimit: java.lang.Long | Null = null
    var timeout: java.time.Duration | Null = null

    val parser = new scopt.OptionParser[Unit]("fuzz_test"):
      arg[String]("class-name")
        .required()
        .foreach(className = _)
        .text("fully qualified name of class to fuzz")
      arg[String]("method-name")
        .required()
        .foreach(methodName = _)
        .text("name of method within the class to fuzz")
      opt[Long]("trial-limit")
        .optional()
        .foreach(trialLimit = _)
        .text("maximum number of trials (default is infinite)")
      opt[Long]("timeout")
        .optional()
        .foreach(millis => timeout = java.time.Duration.ofMillis(millis))
        .text("trial timeout in ms (default is none)")

    if parser.parse(argv, ()).isDefined
    then
      val outputDir = os.pwd / ".fuzz_output" / className / methodName
      if !os.exists(outputDir)
      then os.makeDir.all(outputDir)

      val guidance = new ZestGuidance(
        methodName, // Name of the test method
        timeout,
        trialLimit, // Trial limit (missing if null)
        outputDir.toIO, // Output directory for results
        new Random(), // Random number generator
      )
      println(s"fuzzing $className#$methodName")

      val result = GuidedFuzzing.run(
        className,
        methodName,
        Thread.currentThread().getContextClassLoader(),
        guidance,
        System.out,
      )

      if !result.wasSuccessful()
      then
        println(s"see $outputDir for details")
        sys.exit(1)
    else sys.exit(2)
  end main
end FuzzTestSuiteMain
