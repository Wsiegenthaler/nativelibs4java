package scalacltests

import scala.math._
import scalacl._

import org.junit._
import org.junit.Assert._


/**
 * Some tests to catch trivial issues with random number generation in
 * ScalaCL.  Analysis of the generated sequences is intentionally
 * non-rigorous.
 */
class RandomTest {
    
    implicit val context = Context.best(DoubleSupport)
    
    enforceUsingOpenCL = true
    
    val samplesPerTest = 100000
    val samplesToInspect = 200
        
    
    /** 
     * Utility function - generates and prints random numbers generated
     * using the function of choice 'f' and returns an array of the first 'n' samples.
     */
    def randomTest[T](f:Int=>CLIndexedSeq[T], samples:Int, n:Int=100):Array[T] = {
      val startRandom = System.currentTimeMillis
      val randoms = f(samples)
      val startCopy = System.currentTimeMillis
      val randomArray = randoms.toArray
        
      val subset = randomArray.take(n)
      println( subset.grouped(5).map(_.mkString("\t")).mkString("\n") )
      val endCopy = System.currentTimeMillis
      println("Generated " + samples + " randoms in " + (startCopy - startRandom) + "ms (" + (endCopy - startCopy) + "ms copy, " + (endCopy - startRandom) + "ms total).\n\n")
        
      subset
    }
      
    /** Verifies a set of normalized random numbers lie between 0 and 1 */
    def verifyNormalRange(randoms:Array[Float])  { verifyNormalRange(randoms.map(_.toDouble)) }
    def verifyNormalRange(randoms:Array[Double]) {
       val passed = randoms.filter(r => r >= 0 && r <= 1)
       assertEquals("[ERROR] Not all generated values are between 0 and 1!", randoms.size, passed.size)
    }
       
    /** 
     * Checks a set of normalized random numbers for duplicates, which are
     * generally okay but is a sign of something broken when found in short
     * generated sequences.
     */
    def verifyNoDupes(randoms:Array[Double]) {
       assertEquals("[ERROR] Duplicate values were found in the generated random sequence!", randoms.size, randoms.distinct.size)
    }
    def verifyNoDupes(randoms:Array[Float])  { verifyNoDupes(randoms.map(_.toDouble)) }
    def verifyNoDupes(randoms:Array[Int])  { verifyNoDupes(randoms.map(_.toDouble)) }

    
    /** 
     * Verifies that int's generated with randomInt are between 0 and n-1.
     */
    def verifyIntRange(randoms:Array[Int], nParam:Int) {
       val filtered = randoms.filter(r => r >= 0 && r < nParam)
       assertEquals("[ERROR] Not all generated int's are between 0 and n-1!", randoms.size, filtered.size)
       assertFalse("[ERROR] math.randomInt(n) is not generating unique values!", randoms.distinct.size < 2)
    }
    
    
    /** Uses scala.math.random for doubles */
    @Test
    def testDoubleRandoms1 {
      val randoms = randomTest(generateDoubleRandoms1, samplesPerTest, samplesToInspect)
      verifyNormalRange(randoms)
      verifyNoDupes(randoms)
    }
    
    def generateDoubleRandoms1(samples:Int):CLIndexedSeq[Double] = {
      val randoms = (1 to samples).cl.map { case i => scala.math.random }
      randoms.waitFor
      randoms
    }
    
    
    /** Uses scalacl.math.random version for doubles */
    @Test
    def testDoubleRandoms2 {
      val randoms = randomTest(generateDoubleRandoms2, samplesPerTest, samplesToInspect)
      verifyNormalRange(randoms)
      verifyNoDupes(randoms)
    }

    def generateDoubleRandoms2(samples:Int):CLIndexedSeq[Double] = {
      val randoms = (1 to samples).cl.map { case i => scalacl.math.random }
      randoms.waitFor
      randoms
    }

    
    /** Tests the "double-free" path for systems which only support float */
    @Test
    def testFloatRandoms {
      val randoms = randomTest(generateFloatRandoms, samplesPerTest, samplesToInspect)
      verifyNormalRange(randoms)
      verifyNoDupes(randoms)
    }
        
    def generateFloatRandoms(samples:Int):CLIndexedSeq[Float] = {
      val randoms = (1 to samples).cl.map { case i => scala.math.random.toFloat }
      randoms.waitFor
      randoms
    }
    
    
    /** Tests the scalacl.math version of the "double-free" path */
    @Test
    def testCLFloatRandoms {
      val randoms = randomTest(generateCLFloatRandoms, samplesPerTest, samplesToInspect)
      verifyNormalRange(randoms)
      verifyNoDupes(randoms)
    }
    
    def generateCLFloatRandoms(samples:Int):CLIndexedSeq[Float] = {
      val randoms = (1 to samples).cl.map { case i => scalacl.math.random.toFloat }
      randoms.waitFor
      randoms
    }

    /** Uses scalacl.math.randomInt(n) to generate digits from 0 to n-1 */
    @Test
    def testIntRangeRandoms {
      val n = 25
      val randoms = randomTest(generateIntRangeRandoms(n), samplesPerTest, samplesToInspect)
      verifyIntRange(randoms, n)
    }
    
    def generateIntRangeRandoms(n:Int)(samples:Int) = {
      val randoms = (1 to samples).cl.map { case i => scalacl.math.randomInt(n) }
      randoms.waitFor
      randoms
    }
    
    /** Uses scalacl.math.randomInt to generate random 32-bit unsigned ints */
    @Test
    def testIntRandoms {
      val randoms = randomTest(generateIntRandoms, samplesPerTest, samplesToInspect)
      verifyNoDupes(randoms)
    }
    
    def generateIntRandoms(samples:Int) = {
      val n = 10
      val randoms = (1 to samples).cl.map { case i => scalacl.math.randomInt }
      randoms.waitFor
      randoms
    }

}
