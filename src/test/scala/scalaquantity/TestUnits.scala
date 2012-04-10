package scalaquantity


import scalaquantity.Exponents._
import Units._
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.{FunSuite, FlatSpec}

/**
 * If it compiles, it passed 
 */
class TestUnits extends FunSuite with ShouldMatchers {

  test("An Exponent should support numerals") {
    assert(exponentValue[P0] === 0)
    assert(exponentValue[P1] === 1)
    assert(exponentValue[P4] === 4)
    assert(exponentValue[N3] === -3)
  }

  test("An Exponent should support next and previous") {
    assert(exponentValue[P0#Next] === 1)
    assert(exponentValue[P1#Next] === 2)
    assert(exponentValue[P1#Prev] === 0)
    assert(exponentValue[P0#Next#Prev] === 0)
    assert(exponentValue[P2#Next#Prev#Next#Next] === 4)
  }

  test("An Exponent should support negation") {
    assert(exponentValue[P0#Neg] === 0)
    assert(exponentValue[P3#Neg] === -3)
    assert(exponentValue[P3#Neg#Neg] === 3)
  }

  test("An Exponent should support addition and subtraction") {
    assert(exponentValue[P4#Sub[P3]] === 1)
    assert(exponentValue[P5#Add[P2]] === 7)
  }

  test("A Quantity should have exponents that can be converted to numbers") {
    assert(exponentValue[Speed#M] === 1)
    assert(exponentValue[Speed#S] === -1)
    assert(exponentValue[Speed#KG] === 0)
  }

  test("A Quantity should check units on compile time") {
    val speed: Speed = 120*m / (1*min)
    assert(speed === 2.0*m/s)

    val pressure: Pressure = 10*MN / (10*m2)
    assert(pressure === 10*bar)

    val power: Watt = 9000 * GW
    val tw: Double = power / TW
    assert(tw === 9.0)
  }



}
