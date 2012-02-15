package scalacltests

import scala.math._
import scalacl._
import org.junit._
import org.junit.Assert._


/**
 * Tests various types of symbol captures
 */
class CaptureTest {
    
    implicit val context = Context.best(DoubleSupport)

    val testRange = (1 to 10).cl

    @Test
    def captureLocal {
      val localCapture = 12345
      val results = testRange.map { case i => localCapture }
      assertEquals(localCapture, results(0))
    }

    @Test
    def captureThisMember {
      class Test { val a = 12345; def test = testRange.map { case i => a } }
      val t = new Test()
      val results = t.test
      assertEquals(t.a, results(0))
    }

    @Test
    def captureObjectVal {
      val results = testRange.map { case i => CaptureTest.objectVal }
      assertEquals(CaptureTest.objectVal, results(0))
    }

    @Test
    def captureConstructorVal {
      class Test(val a:Int) { def test = testRange.map { case i => a } }
      val testVal = 12345
      
      val results = new Test(testVal).test
      assertEquals(testVal, results(0))
    }

// This breaks compilation, fix and uncomment!
//    @Test
//    def captureConstructorField {
//      class Test(b:Int) { def test = testRange.map { case i => b } }
//      val testVal = 12345
//      
//      val results = new Test(testVal).test
//      assertEquals(testVal, results(0))
//    }

    @Test
    def captureScalaPi {
      val results = testRange.map { case i => scala.math.Pi }
      assertTrue( abs(scala.math.Pi - results(0)) < 0.001 )
    }

}

object CaptureTest {
  val objectVal = 12345
}
