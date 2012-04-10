package scalaquantity

import scalaquantity.Units._

/**
 * Tests units.  If this compiles, then the test is successful.
 */
object VelociraptorExample {


  def main(args: Array[String]) {
    val velociraptorSpeed = 25 * m / s
    val humanRunningSpeed = 10 * km / h
    val distance = 30 * m

    println("Assume a velociraptor with a velocity of " + velociraptorSpeed + " m/s " +
            "and a human with a running speed of "+ humanRunningSpeed + " m/s " +
            "at a distance " + distance +" m from each other.  \n" +
            "For how long can the human outrun the velociraptor?  How far will the human get?")

    Thread.sleep(16000) // Dramatic pause!
    println("Answer:")

    val humanRemainingLifeTime: Time = distance / (velociraptorSpeed - humanRunningSpeed)
    println("The human can escape for " + humanRemainingLifeTime + " s")

    println("Human movement distance: " + distanceAtSpeed (0*m, humanRunningSpeed, humanRemainingLifeTime) + " m")
    println("Velociraptor movement distance: " + distanceAtSpeed (0*m, velociraptorSpeed, humanRemainingLifeTime) + " m (that's one fast velociraptor!)")
  }

  def distanceAtSpeed(pos: m, velocity: m/s, duration: s): m = pos + velocity * duration
}