package scalaquantity

import scalaquantity.Units._

/**
 * Tests units.  If this compiles, then the test is successfull.
 */
object TestUnits {

  def main(args: Array[String]) {

    val velociraptorSpeed = 25 * m / s
    val humanRunningSpeed = 10 * km / h
    val distance = 30 * feet
    val humanRemainingLifeTime: Time = distance / (velociraptorSpeed - humanRunningSpeed)
    println("Seconds left to live: " + humanRemainingLifeTime)
  }

}