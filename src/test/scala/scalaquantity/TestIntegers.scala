package scalaquantity


import Exponents._
import Utils._

/**
 * If it compiles, it passed 
 */
object TestIntegers {
  def main(args: Array[String]) {

    val t1 : p5#Sub[p2] = new p3
    val t2 : p3#Add[p1] = new p4
    val t3 : p2#Neg = new p0#Prev#Prev
    val t4 : p2 = new p0#Next#Next
    val t5 : p3#Sub[p5]#Neg = new p2

    println(to[p4, Int])
    println(to[m3, Int])
    println(to[p0, Int])



    assert(to[p4, Int] == 4)


/*
    trait Base[_M <: Exp, _KG <: Exp, _S <: Exp] {
      type M = _M
      type KG = _KG
      type S = _S
    }

    type Speed = Base[p1, __, m1]

    case class Derived[M_, KG_, S_]() extends Base[M_, KG_, S_]{
      
      assert(to[M, Int] == 1)
      assert(to[S, Int] == -1)
      println(to[M, Int])
      println(to[S, Int])
    }

    new Derived[p1, __, m1]()
*/

  }
}