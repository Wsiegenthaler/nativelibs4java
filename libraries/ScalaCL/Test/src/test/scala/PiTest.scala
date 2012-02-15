package scalacltests

import scala.math._
import scalacl._

import org.junit._
import org.junit.Assert._

class PiTest {
    
    implicit val context = Context.best(DoubleSupport)

    enforceUsingOpenCL = true
        
    
    @Test
    def estimatePiTest = {
      val results = Array(
          (10000, 100),
          (100000, 1000),
          (100000, 10000),
          (1000000, 10000),
          (10000000, 100000)
        ) map { case (samples, samplesPerStream) =>
          val startPi = System.currentTimeMillis
          val pi = estimatePi(samples, samplesPerStream)
          println(" ->  pi=" + pi + " in " + (System.currentTimeMillis-startPi) + "ms with " + samples + " samples (" + samplesPerStream + " per stream).")
          pi
      }
      
      // Verify that all results approach pi
       val error = results.map(pi => abs(scala.math.Pi - pi) )
       assertTrue( error.filter(e => e < 0.05).size == results.size )
    }
    
    /**
     * Test kernel calculates pi by generating random 2d samples and comparing
     * the ratio of samples landing on a square vs an inscribed circle.  Pi is then
     * calculated by relating the equations for area of a circle and
     * square, solving for pi symbolically, and using the ratio calculated during
     * sampling to approximate a value.
     */
    def estimatePi(samples:Int, samplesPerStream:Int):Double = {
      val workUnits = samples / samplesPerStream
      val workRange = (1 to workUnits).cl
      val pi = workRange.map { case i =>
        var total = 0d
        for (j <- 1 to samplesPerStream) {
          val x = 2 * random - 1
          val y = 2 * random - 1
          val h = x*x + y*y
          if (h <= 1)  total = total + 1
        }
      total
      }.sum * 4 / samples
      
      pi
    }

}
