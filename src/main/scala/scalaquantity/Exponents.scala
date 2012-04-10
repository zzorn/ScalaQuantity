package scalaquantity




object Exponents {

  type -[S1 <: Exp, S2 <: Exp] = S1#Sub[S2]
  type +[A1 <: Exp, A2 <: Exp] = A1#Add[A2]

  sealed trait Exp {
    type Add[AI <: Exp] <: Exp
    type Sub[SI <: Exp] <: Exp
    type Neg <: Exp
    type Next <: Exp
    type Prev <: Exp
  }

  sealed trait NatExp extends Exp

  /** Zero exponent */
  final class P0 extends NatExp {
    type Add[I <: Exp] = I
    type Sub[I <: Exp] = I#Neg
    type Neg = P0
    type Next = NextExp[P0]
    type Prev = Next#Neg
  }


  sealed trait PosExp extends NatExp
  final class NextExp[P <: NatExp] extends PosExp {
    type This = NextExp[P]
    type Add[N <: Exp] = P#Add[N]#Next
    type Sub[N <: Exp] = P#Sub[N]#Next
    type Neg = NegExp[This]
    type Prev = P
    type Next = NextExp[This]
  }

  final class NegExp[N <: PosExp] extends Exp {
    type Add[A <: Exp] = N#Sub[A]#Neg
    type Sub[A <: Exp] = N#Add[A]#Neg
    type Neg = N
    type Next = N#Prev#Neg
    type Prev = N#Next#Neg
  }

  // Alias zero exponent to double underscores, to allow clearer unit definitions
  type __ = P0

  // Positive exponents
  type P1 = NextExp[P0]
  type P2 = NextExp[P1]
  type P3 = NextExp[P2]
  type P4 = NextExp[P3]
  type P5 = NextExp[P4]
  type P6 = NextExp[P5]
  type P7 = NextExp[P6]
  type P8 = NextExp[P7]
  type P9 = NextExp[P8]
  type P10 = NextExp[P9]

  // Negative exponents
  type N1 = P1#Neg
  type N2 = P2#Neg
  type N3 = P3#Neg
  type N4 = P4#Neg
  type N5 = P5#Neg
  type N6 = P6#Neg
  type N7 = P7#Neg
  type N8 = P8#Neg
  type N9 = P9#Neg
  type N10 = P10#Neg


  case class ExponentToValue[T](value : Int)
  implicit val p0ToInt = ExponentToValue[P0](0)
  implicit def nextToInt[P <: NatExp](implicit v : ExponentToValue[P]) = ExponentToValue[NextExp[P]]( 1 + v.value )
  implicit def negToInt [P <: PosExp](implicit v : ExponentToValue[P]) = ExponentToValue[NegExp[P] ](    -v.value )

  /**
   * @return the integer value of the specified unit exponent.
   * E.g. exponentValue[Speed#M] == 1 and exponentValue[Speed#S] == -1
   */
  def exponentValue[T](implicit exponentToValue: ExponentToValue[T]): Int = exponentToValue.value


}