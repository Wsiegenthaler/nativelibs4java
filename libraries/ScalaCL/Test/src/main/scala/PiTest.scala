object PiTest {
    import TestUtils._
    import scala.math._
    import scalacl._
    
    implicit val context = Context.best(DoubleSupport) // prefer CPUs ? use Context.best(CPU)
    
    def main(args: Array[String]): Unit = {
      Array(
          (10000, 100),
          (100000, 1000),
          (100000, 10000),
          (1000000, 10000),
          (10000000, 100000)
        ) map { case (samples, samplesPerStream) =>
          val startPi = System.currentTimeMillis
          val pi1 = estimatePi(samples, samplesPerStream)
          println(" ->  pi=" + pi1 + " in " + (System.currentTimeMillis-startPi) + "ms with " + samples + " samples (" + samplesPerStream + " per stream).")
      }
    }
    
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
