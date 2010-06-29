package scalaquantity

object Units {
  import Integers._
  import Utils._
  import Addables._
  import Subtractables._

  trait Unit {
    type M <: MInt
    type KG <: MInt
    type S <: MInt
    type A <: MInt
    type K <: MInt
    type Mol <: MInt
    type CD <: MInt
  }
  
  final class TUnit[_M <: MInt, _KG <: MInt, _S <: MInt, _A <: MInt, _K <: MInt, _Mol <: MInt, _CD <: MInt] {
    type M = _M
    type KG = _KG
    type S = _S
    type A = _A
    type K = _K
    type Mol = _Mol
    type CD = _CD
  }

  case class Quantity[M <: MInt, KG <: MInt, S <: MInt, A <: MInt, K <: MInt, Mol <: MInt, CD <: MInt](value : Double) {
    type This = Quantity[M, KG, S, A, K, Mol, CD]
    def +(m : This) = Quantity[M, KG, S, A, K, Mol, CD](value + m.value)
    def -(m : This) = Quantity[M, KG, S, A, K, Mol, CD](value - m.value)
    def *[M2 <: MInt, KG2 <: MInt, S2 <: MInt, A2 <: MInt, K2 <: MInt, Mol2 <: MInt, CD2 <: MInt](m : Quantity[M2, KG2, S2, A2, K2, Mol2, CD2]) = Quantity[M + M2, KG + KG2, S + S2, A + A2, K + K2, Mol + Mol2, CD + CD2](value * m.value)
    def /[M2 <: MInt, KG2 <: MInt, S2 <: MInt, A2 <: MInt, K2 <: MInt, Mol2 <: MInt, CD2 <: MInt](m : Quantity[M2, KG2, S2, A2, K2, Mol2, CD2]) = Quantity[M - M2, KG - KG2, S - S2, A - A2, K - K2, Mol - Mol2, CD - CD2](value * m.value)
    def apply(v : Double) = Quantity[M, KG, S, A, K, Mol, CD](v * value)
  }

  implicit def measure(v : Double) = Quantity[_0, _0, _0, _0, _0, _0, _0](v)

  type Length = Quantity[_1, _0, _0, _0, _0, _0, _0]
  type Mass = Quantity[_0, _1, _0, _0, _0, _0, _0]
  type Time = Quantity[_0, _0, _1, _0, _0, _0, _0]
  type Current = Quantity[_0, _0, _0, _1, _0, _0, _0]
  type Temperature = Quantity[_0, _0, _0, _0, _1, _0, _0]
  type Mol = Quantity[_0, _0, _0, _0, _0, _1, _0]
  type LuminousIntensity = Quantity[_0, _0, _0, _0, _0, _0, _1]

  type Angle = Quantity[_0, _0, _0, _0, _0, _0, _0]
  type Area = Quantity[_2, _0, _0, _0, _0, _0, _0]
  type Volume = Quantity[_3, _0, _0, _0, _0, _0, _0]
  type Speed = Quantity[_1, _0, _1#Neg, _0, _0, _0, _0]
  type Acceleration = Quantity[_1, _0, _2#Neg, _0, _0, _0, _0]
  type Frequency = Quantity[_0, _0, _1#Neg, _0, _0, _0, _0]
  type Force = Quantity[_1, _1, _2#Neg, _0, _0, _0, _0]
  type Energy = Quantity[_2, _1, _2#Neg, _0, _0, _0, _0]
  type Power = Quantity[_2, _1, _3#Neg, _0, _0, _0, _0]
  type Pressure = Quantity[_1#Neg, _1, _2#Neg, _0, _0, _0, _0]
  type ElectricPotential = Quantity[_2, _1, _3#Neg, _1#Neg, _0, _0, _0]

  val m = Length(1)
  val kg = Mass(1)
  val s = Time(1)
  val A = Current(1)
  val K = Temperature(1)
  val mol = Mol(1)
  val cd = LuminousIntensity(1)

  val V = ElectricPotential(1)
  val N = Force(1)
  val Pa = Pressure(1)
  val Hz = Frequency(1)
  val J = Energy(1)
  val W = Power(1)

  val m2 = Area(1)
  val m3 = Volume(1)


}