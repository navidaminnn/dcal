package distcompiler

import edu.berkeley.cs.jqf.fuzz.junit.GuidedFuzzing
import edu.berkeley.cs.jqf.fuzz.guidance.Guidance
import edu.berkeley.cs.jqf.fuzz.guidance.Result
import edu.berkeley.cs.jqf.fuzz.ei.ZestGuidance

import java.io.File
import java.io.PrintStream
import java.util.Random

object RunFuzzLib {
  def main(args: Array[String]): Unit = {
    val testClassName = "distcompiler.FuzzLibTests"
    val testMethod = "simpleTest"
    
    val outputDir = new File("fuzz-output")
    if (!outputDir.exists()) {
      outputDir.mkdirs()
    }
    
    val guidance = new ZestGuidance(
      testMethod,                // Name of the test method
      null,                      // Duration (null for unlimited time)
      10L,                       // Trial limit
      outputDir,                 // Output directory for results
      new Random()               // Random number generator
    )
    
    println(s"Running JQF test: $testClassName#$testMethod")
    println(s"Output directory: ${outputDir.getAbsolutePath}")
    
    val classLoader = Thread.currentThread().getContextClassLoader()
    
    val result = GuidedFuzzing.run(
      testClassName,
      testMethod,
      classLoader,
      guidance,
      System.out
    )
    
    if (result.wasSuccessful()) {
      println("Test completed successfully without finding failures.")
    } else {
      println(s"Test found ${result.getFailureCount()} failures!")
      result.getFailures().forEach { failure =>
        println(s"Failure: ${failure.getMessage()}")
        failure.getException().printStackTrace()
      }
    }
    
    // println(s"Total runs: ${guidance.getNumTrials()}")
    // println(s"Valid inputs: ${guidance.getNumValid()}")
    // println(s"Unique failures: ${guidance.getFailures().size()}")
  }
}
