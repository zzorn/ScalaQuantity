package scalaquantity

import scalaquantity.Units._
import scala.math.Pi

/**
 * Tests units.  If this compiles, then the test is successful.
 */
object FoodExample {

  def main(args: Array[String]) {

    val appleRadius = 3*cm
    val appleEnergyDensity = 52 * cal / (100*g)
    val appleDensity =  1150 * kg/m3
    val bobWeigh = 160 * lb
    val bobWalkSpeed = 3 * miles/h
    val walkEnergy = 85  * cal/mile // Source: http://walking.about.com/cs/howtoloseweight/a/howcalburn.htm

    println("Bob is stranded in a desert.  He has one apple with a radius of "+appleRadius+" m and a density of " + appleDensity + " kg/m3\n" +
            "Apples have an energy density of " + appleEnergyDensity + " J/kg.\n"+
            "Bob burns " + walkEnergy / (cal/mile) + " calories per mile when walking.\n" +
            "Bob walks at " + bobWalkSpeed / (miles/h) + " miles/h.\n" +
            "How far will Bob walk on the apple?  How long?")

    Thread.sleep(20000) // Dramatic pause!
    println("Answer:")

    val appleVolume: Volume =  4.0/3.0 * Pi * appleRadius*appleRadius*appleRadius
    val appleMass: Mass = appleVolume * appleDensity
    val appleEnergy: Energy = appleMass * appleEnergyDensity
    val bobWalkDistance: Length = appleEnergy / walkEnergy
    val bobWalkTime: Time = bobWalkDistance / bobWalkSpeed

    println("Bob will walk " + bobWalkDistance / km + " km for "+bobWalkTime/min+" min on the " +appleEnergy +" J of energy in the "+appleMass/g+" g apple.")

  }

}