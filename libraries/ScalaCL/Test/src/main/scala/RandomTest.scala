object RandomTest {
    import TestUtils._
    import scala.math._
    import scalacl._
    
    implicit val context = Context.best(DoubleSupport) // prefer CPUs ? use Context.best(CPU)
    
    def main(args: Array[String]): Unit = {
      Array(10000, 100000, 100000, 1000000, 10000000) map { case samples =>
        val startRandom = System.currentTimeMillis
        val randoms = generateRandoms(samples)
          
        val startCopy = System.currentTimeMillis
        val randomArray = randoms.toArray
        
        println( randomArray.take(100).grouped(5).map(_.mkString("\t")).mkString("\n") )
        val endCopy = System.currentTimeMillis
        println("Generated " + samples + " randoms in " + (startCopy - startRandom) + "ms (" + (endCopy - startCopy) + "ms copy, " + (endCopy - startRandom) + "ms total).\n\n")
      }
    }
    
    def generateRandoms(samples:Int) = {
      val randoms = (1 to samples).cl.map { case i => random }
      randoms.waitFor
      randoms
    }
}
