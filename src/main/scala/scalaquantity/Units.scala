package scalaquantity

import scalaquantity.Utils.TypeToValue

import Exponents._
import Utils._

import Exponents._
import Exponents.succToInt
import Exponents.negToInt
import Exponents.p0ToInt


object Units {

/*
  trait Unit {
    type M <: Exp
    type KG <: Exp
    type S <: Exp
    type A <: Exp
    type K <: Exp
    type Mol <: Exp
    type CD <: Exp
  }

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

  case class Quantity[M <: Exp, KG <: Exp, S <: Exp, A <: Exp, K <: Exp, Mol <: Exp, CD <: Exp](value: Double) {
    type This = Quantity[M, KG, S, A, K, Mol, CD]
    def +(m : This) = Quantity[M, KG, S, A, K, Mol, CD](value + m.value)
    def -(m : This) = Quantity[M, KG, S, A, K, Mol, CD](value - m.value)
    def *[M2 <: Exp, KG2 <: Exp, S2 <: Exp, A2 <: Exp, K2 <: Exp, Mol2 <: Exp, CD2 <: Exp](m : Quantity[M2, KG2, S2, A2, K2, Mol2, CD2]) = Quantity[M + M2, KG + KG2, S + S2, A + A2, K + K2, Mol + Mol2, CD + CD2](value * m.value)
    def /[M2 <: Exp, KG2 <: Exp, S2 <: Exp, A2 <: Exp, K2 <: Exp, Mol2 <: Exp, CD2 <: Exp](m : Quantity[M2, KG2, S2, A2, K2, Mol2, CD2]) = Quantity[M - M2, KG - KG2, S - S2, A - A2, K - K2, Mol - Mol2, CD - CD2](value * m.value)
    def apply(v : Double) = Quantity[M, KG, S, A, K, Mol, CD](v * value)

    override def toString = ""+value
  }
  
  implicit def measure(v : Double) = Quantity[__, __, __, __, __, __, __](v)

  type Length             = Quantity[p1, __, __, __, __, __, __]
  type Mass               = Quantity[__, p1, __, __, __, __, __]
  type Time               = Quantity[__, __, p1, __, __, __, __]
  type Current            = Quantity[__, __, __, p1, __, __, __]
  type Temperature        = Quantity[__, __, __, __, p1, __, __]
  type AmountOfSubstance  = Quantity[__, __, __, __, __, p1, __]
  type LuminousIntensity  = Quantity[__, __, __, __, __, __, p1]

  // Some derived types
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
  type Voltage            = Quantity[p2, p1, m3, m1, __, __, __]
  type ElectricCharge     = Quantity[__, __, p1, p1, __, __, __]
  type Capacitance        = Quantity[m2, m1, p4, p2, __, __, __]
  type ElectricResistance = Quantity[p2, p1, m3, m2, __, __, __]
  type Inductance         = Quantity[p2, p1, m2, m2, __, __, __]
  type Illuminance        = Quantity[m2, __, __, __, __, __, p1]
  type CatalyticActivity  = Quantity[__, __, m1, __, __, p1, __]


  /** See http://tauday.com/ */
  private val Tau = 2*Math.Pi

  val tera = 1e12
  val giga = 1e9
  val mega = 1e6
  val kilo = 1e3
  val deci = 1e-1
  val centi = 1e-2
  val milli = 1e-3
  val micro = 1e-6
  val nano = 1e-9
  val pico = 1e-12

  // Unitless values (angles)
  /** Unit of radians.  E.g. 2*Pi*radians = 360 degrees. */
  val rad   = new Angle(1)
  /** Unit of turns. E.g. 0.5*turns = 180 degrees */
  val turns = new Angle(Tau)
  /** Unit of degrees. E.g. 90*degrees = 0.25 turns*/
  val deg   = new Angle(360.0/Tau)

  // Length
  /** Meter */
  val m    = new Length(1)
  val km   = m(kilo)
  val dm   = m(deci)
  val cm   = m(centi)
  val mm   = m(milli)
  val um   = m(micro)
  val nm   = m(nano)

  // Mass
  val g    = new Mass(1e-3)
  val kg   = g(kilo)
  val mg   = g(milli)
  val ug   = g(micro)
  /** A metric ton = 1000 kg */
  val ton  = kg(1000)

  // Time
  /** Second */
  val s    = new Time(1)
  val ms   = s(milli)
  val us   = s(micro)
  val ns   = s(nano)
  /** Minute */
  val min  = s(60)
  val hour = min(60)
  val day  = hour(24)
  /** Julian year, defined as 365.25 days. */
  val year = day(365.25)

  // Electric current
  val A    = new Current(1)
  val kA   = A(kilo)
  val mA   = A(milli)
  val uA   = A(micro)

  // Temperature
  val K   = new Temperature(1)
  val mK  = K(milli)
  private val zeroCelsiusInKelvin = 273.15
  /** The temperature of zero celsius in kelvins. */
  val zeroCelsius = new Temperature(zeroCelsiusInKelvin)
  def fromCelsius(value: Double): Temperature = new Temperature(value + zeroCelsiusInKelvin)
  def toCelsius(temperature: Temperature): Double = temperature.value - zeroCelsiusInKelvin

  // Amount of substance
  val mol = new AmountOfSubstance(1)

  // Luminous intensity
  val cd  = new LuminousIntensity(1)
  val lumen = cd

  // Derived untis

  val V   = new Voltage(1)
  val GV   = V(giga)
  val MV   = V(mega)
  val kV   = V(kilo)
  val mV   = V(milli)
  val uV   = V(micro)

  val N   = new Force(1)
  val GN  = N(giga)
  val MN  = N(mega)
  val kN  = N(kilo)
  val mN  = N(milli)
  val uN  = N(micro)
  
  val Pa  = new Pressure(1)
  val GPa = Pa(giga)
  val MPa = Pa(mega)
  val kPa = Pa(kilo)
  val mPa = Pa(milli)
  val uPa = Pa(micro)

  val Hz  = new Frequency(1)
  val GHz = Hz(giga)
  val MHz = Hz(mega)
  val kHz = Hz(kilo)
  val mHz = Hz(milli)
  val uHz = Hz(micro)
  
  val J   = new Energy(1)
  val GJ  = J(giga)
  val MJ  = J(mega)
  val kJ  = J(kilo)
  val mJ  = J(milli)
  val uJ  = J(micro)

  val W   = new Power(1)
  val TW  = W(tera)
  val GW  = W(giga)
  val MW  = W(mega)
  val kW  = W(kilo)
  val mW  = W(milli)
  val uW  = W(micro)

  val C   = new ElectricCharge(1)
  val Coulomb = C

  val F   = new Capacitance(1)
  val kF  = F(kilo)
  val mF  = F(milli)
  val uF  = F(micro)
  val nF  = F(nano)
  val pF  = F(pico)

  val Ohm  = new ElectricResistance(1)
  val GOhm = Ohm(giga)
  val MOhm = Ohm(mega)
  val kOhm = Ohm(kilo)
  val mOhm = Ohm(milli)
  val uOhm = Ohm(micro)

  val H    = new Inductance(1)
  val GH   = H(giga)
  val MH   = H(mega)
  val kH   = H(kilo)
  val mH   = H(milli)
  val uH   = H(micro)
  val nH   = H(nano)

  val lux  = new Illuminance(1)

  val kat  = new CatalyticActivity(1)


  val m2  = new Area(1)

  val m3  = new Volume(1)
  val litre = dm*dm*dm

  val s2 = s*s
  val s3 = s*s*s


  // TODO: Would be nice if the types could also be expressed as e.g. def foo(param: m/s).  Should probably work given inlined types..

}