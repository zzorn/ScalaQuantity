package scalaquantity

import scalaquantity.Utils.TypeToValue

object Exponents {

//  type -[N <: MInt] = N#Neg
  type -[S1 <: Exp, S2 <: Exp] = S1#Sub[S2]
  type +[A1 <: Exp, A2 <: Exp] = A1#Add[A2]

  sealed trait Exp {
    type Add[AI <: Exp] <: Exp
    type Sub[SI <: Exp] <: Exp
    type Neg <: Exp
    type Next <: Exp
    type Prev <: Exp

/*
    def value: Int
*/
  }

  sealed trait NatExp extends Exp

  /** Zero exponent */
  final class p0 extends NatExp {
    type Add[I <: Exp] = I
    type Sub[I <: Exp] = I#Neg
    type Neg = p0
    type Next = NextExp[p0]
    type Prev = Next#Neg

/*
    def value: Int = 0
*/
  }

  sealed trait PosExp extends NatExp
  final class NextExp[P <: NatExp] extends PosExp {
    type This = NextExp[P]
    type Add[N <: Exp] = P#Add[N]#Next
    type Sub[N <: Exp] = P#Sub[N]#Next
    type Neg = NegExp[This]
    type Prev = P
    type Next = NextExp[This]

/*
    def value: Int = new P().value + 1
*/
  }

  final class NegExp[N <: PosExp] extends Exp {
    type Add[N <: Exp] = N#Add[N#Neg]#Neg
    type Neg = N
    type Next = N#Prev#Neg
    type Prev = N#Next#Neg

/*
    def value: Int = -(new N().value )
*/
  }

  // Alias zero exponent to double undersocres, to allow clearer unit definitions
  type __ = p0

  // Positive exponents
  type p1 = NextExp[p0]
  type p2 = NextExp[p1]
  type p3 = NextExp[p2]
  type p4 = NextExp[p3]
  type p5 = NextExp[p4]
  type p6 = NextExp[p5]
  type p7 = NextExp[p6]
  type p8 = NextExp[p7]
  type p9 = NextExp[p8]
  type p10 = NextExp[p9]

  // Negative exponents
  type m1 = p1#Neg
  type m2 = p2#Neg
  type m3 = p3#Neg
  type m4 = p4#Neg
  type m5 = p5#Neg
  type m6 = p6#Neg
  type m7 = p7#Neg
  type m8 = p8#Neg
  type m9 = p9#Neg
  type m10 = p10#Neg

  val e0 = new p0
  val e1 = new p1
  val e2 = new p2
  val e3 = new p3
  val e4 = new p4
  val e5 = new p5
  val e6 = new p6
  val e7 = new p7
  val e8 = new p8
  val e9 = new p9
  val e10 = new p10

  implicit val _0ToInt = TypeToValue[p0, Int](0)
  implicit def succToInt[P <: NatExp](implicit v : TypeToValue[P, Int]) = TypeToValue[NextExp[P], Int](1 + v.value)

}