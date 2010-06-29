package scalaquantity

object Units {
  import Exponents._

/* TODO: Where and for what is this used?
  trait Unit {
    type M <: Exp
    type KG <: Exp
    type S <: Exp
    type A <: Exp
    type K <: Exp
    type Mol <: Exp
    type CD <: Exp
  }
*/

/* TODO: Where is this used?  Or for what?
  final class TUnit[_M <: Exp, _KG <: Exp, _S <: Exp, _A <: Exp, _K <: Exp, _Mol <: Exp, _CD <: Exp] {
    type M = _M
    type KG = _KG
    type S = _S
    type A = _A
    type K = _K
    type Mol = _Mol
    type CD = _CD
  }
*/

  case class Quantity[M <: Exp, KG <: Exp, S <: Exp, A <: Exp, K <: Exp, Mol <: Exp, CD <: Exp](value : Double) {
    type This = Quantity[M, KG, S, A, K, Mol, CD]
    def +(m : This) = Quantity[M, KG, S, A, K, Mol, CD](value + m.value)
    def -(m : This) = Quantity[M, KG, S, A, K, Mol, CD](value - m.value)
    def *[M2 <: Exp, KG2 <: Exp, S2 <: Exp, A2 <: Exp, K2 <: Exp, Mol2 <: Exp, CD2 <: Exp](m : Quantity[M2, KG2, S2, A2, K2, Mol2, CD2]) = Quantity[M + M2, KG + KG2, S + S2, A + A2, K + K2, Mol + Mol2, CD + CD2](value * m.value)
    def /[M2 <: Exp, KG2 <: Exp, S2 <: Exp, A2 <: Exp, K2 <: Exp, Mol2 <: Exp, CD2 <: Exp](m : Quantity[M2, KG2, S2, A2, K2, Mol2, CD2]) = Quantity[M - M2, KG - KG2, S - S2, A - A2, K - K2, Mol - Mol2, CD - CD2](value * m.value)
    def apply(v : Double) = Quantity[M, KG, S, A, K, Mol, CD](v * value)


    override def toString = value + " " + unitToString
    
    def unitToString: String = {

      def composeUnit[E <: Exp](name: String): String = {
/*
        val exponent = new E().value
*/
        // TODO: Get value
        val exponent = "?"
        if (exponent != 0) name + exponent else ""
      }
      
      composeUnit[M]("m") +
      composeUnit[KG]("kg") +
      composeUnit[S]("s") +
      composeUnit[A]("A") +
      composeUnit[K]("K") +
      composeUnit[Mol]("mol") +
      composeUnit[CD]("cd")
    }
  }

  implicit def measure(v : Double) = Quantity[__, __, __, __, __, __, __](v)

  type Length             = Quantity[p1, __, __, __, __, __, __]
  type Mass               = Quantity[__, p1, __, __, __, __, __]
  type Time               = Quantity[__, __, p1, __, __, __, __]
  type Current            = Quantity[__, __, __, p1, __, __, __]
  type Temperature        = Quantity[__, __, __, __, p1, __, __]
  type Mol                = Quantity[__, __, __, __, __, p1, __]
  type LuminousIntensity  = Quantity[__, __, __, __, __, __, p1]

  type Angle              = Quantity[__, __, __, __, __, __, __]
  type Area               = Quantity[p2, __, __, __, __, __, __]
  type Volume             = Quantity[p3, __, __, __, __, __, __]
  type Speed              = Quantity[p1, __, m1, __, __, __, __]
  type Acceleration       = Quantity[p1, __, m2, __, __, __, __]
  type Frequency          = Quantity[__, __, m1, __, __, __, __]
  type Force              = Quantity[p1, p1, m2, __, __, __, __]
  type Energy             = Quantity[p2, p1, m2, __, __, __, __]
  type Power              = Quantity[p2, p1, m3, __, __, __, __]
  type Pressure           = Quantity[m1, p1, m2, __, __, __, __]
  type ElectricPotential  = Quantity[p2, p1, m3, m1, __, __, __]

  val m   = new Length(1)
  val kg  = new Mass(1)
  val s   = new Time(1)
  val A   = new Current(1)
  val K   = new Temperature(1)
  val mol = new Mol(1)
  val cd  = new LuminousIntensity(1)

  val V   = new ElectricPotential(1)
  val N   = new Force(1)
  val Pa  = new Pressure(1)
  val Hz  = new Frequency(1)
  val J   = new Energy(1)
  val W   = new Power(1)

  val m2  = new Area(1)
  val m3  = new Volume(1)


}