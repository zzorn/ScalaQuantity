package scalaquantity

import scalaquantity.Units._

/**
 * Tests units.  If this compiles, then the test is successfull.
 */
object TestUnits {

  def main(args: Array[String]) {

    println(new Speed(1))

    val speed1: Speed = new Speed(10)
    val speed2: Speed = 10 * m/s


    val foo = 100*N * 3*J
    println(foo)
  }

  
}