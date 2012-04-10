= ScalaQuantity =

Library for expressing quantities and units of measurement in Scala in a type safe manner, where the compiler checks that assignments and calculations with the units are correct.


== Usage ==

import scalaquantity.Units._

object VelociraptorTest {
  def main(args: Array[String]) {

    val velociraptorSpeed = 25 * m / s
    val humanRunningSpeed = 10 * km / h
    val distance = 30 * feet

    val humanRemainingLifeTime: Time = distance / (velociraptorSpeed - humanRunningSpeed)

    println("Seconds left to live: " + humanRemainingLifeTime)
  } 
}


For available units, see:
https://github.com/zzorn/ScalaQuantity/blob/master/src/main/scala/scalaquantity/Units.scala

For more examples, see:
https://github.com/zzorn/ScalaQuantity/tree/master/src/test/scala/scalaquantity


=== Gotchas ===
IDE / Compile type errors will not be very readable, as ScalaQuantity is using type magic for the compile-time validation.  
On the other hand, the errors will be caught at compile and not runtime.

You may need to parenthesize some expressions more than would first be intuitive, e.g.
val speed: Speed = 100 * miles / 2 * hours   // Compile error: Will evaluate to 50 mile hours, instead of 50 miles per hour.
val speed: Speed = 100 * miles / (2 * hours) // Compiles ok, speed is 50 miles/hour.


== Credits ==

Author: Hans Häggström, zzorn at iki dot fi

Based on and inspired by existing code for Church Numerals in Scala.
See e.g.: http://jim-mcbeath.blogspot.com/2008/11/practical-church-numerals-in-scala.html

Contains some code adapted from the Metascala library (http://www.assembla.com/wiki/show/metascala) by Jesper Nordenberg.
The main difference of ScalaQuantity and Metascala is that ScalaQuantity focuses on just physical units. 


== License ==

Licensed under the BSD license.
