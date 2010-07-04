package scalaquantity


import Exponents._
import Utils._

/**
 * If it compiles, it passed 
 */
object TestIntegers {
  def main(args: Array[String]) {

    val t1 : P5#Sub[P2] = new P3
    val t2 : P3#Add[P1] = new P4
    val t3 : P2#Neg = new P0#Prev#Prev
    val t4 : P2 = new P0#Next#Next
    val t5 : P3#Sub[P5]#Neg = new P2
    
    // Subtraction, addition, at and around zero in both directions
    val t6 : P0 = new P0#Sub[P0]
    val t7 : P1 = new P1#Sub[P0]
    val t8 : N1 = new P0#Sub[P1]
    val t9 : N1 = new N1#Sub[P0]
    val t10 : N2 = new P3#Sub[P5]
    val t11 : P3 = new N2#Add[P5]
    val t12 : N7 = new N2#Sub[P5]
    val t13 : P1 = new N2#Sub[N3]

    println(to[P4, Int])
    println(to[N3, Int])
    println(to[P0, Int])

    assert(to[P4, Int] == 4)

  }
}