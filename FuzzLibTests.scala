package distcompiler

import edu.berkeley.cs.jqf.fuzz.Fuzz
import edu.berkeley.cs.jqf.fuzz.JQF
import org.junit.runner.RunWith
import org.junit.Assert.*

@RunWith(classOf[JQF])
class FuzzLibTests:
  private val fuzzLib = new FuzzLib()
  
  @Fuzz
  def simpleTest(flag: Boolean, value: Int): Unit = 
    try
      val result = fuzzLib.fillerFunction(flag, value)
      
      if !flag then
        fail("Expected exception was not thrown")
      else
        if value > 0 then 
          assertTrue("Positive value", result > 0)
        else
          assertTrue("Non-positive value", result <= 0)
    catch
      case e: RuntimeException =>
        assertTrue("Exception thrown when flag was true", !flag)
        assertEquals("Found a failure case", e.getMessage)