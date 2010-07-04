package scalaquantity

import scalaquantity.Units._

/**
 * Tests units.  If this compiles, then the test is successfull.
 */
object TestUnits {

  def main(args: Array[String]) {

    val speed1 = m(1) / (2*s)
    val speed2 : m/s = m(1) / s(2)
    val speed3 : Speed = m(1) / s(2)
    val speed4 : Length/Time = m(1) / s(2)



    val velociraptorSpeed = 25 * m / s
    val humanRunningSpeed = 10 * km / h
    val distance = 30 * m
    val humanRemainingLifeTime: Time = distance / (velociraptorSpeed - humanRunningSpeed)
    println("Seconds left to live: " + humanRemainingLifeTime)

    println("Human movement distance: " + distanceAtSpeed (0*m, humanRunningSpeed, humanRemainingLifeTime))
    println("Velociraptor movement distance: " + distanceAtSpeed (0*m, velociraptorSpeed, humanRemainingLifeTime))

    
  }

  def distanceAtSpeed(pos: m, velocity: m/s, duration: s): m = pos + velocity * duration

}