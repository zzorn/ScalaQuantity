package scalaquantity

import Exponents._
import scala.math.Pi


object Units {

  trait Unit {
    type M <: Exp
    type KG <: Exp
    type S <: Exp
    type A <: Exp
    type K <: Exp
    type Mol <: Exp
    type CD <: Exp

    type Div[O <: Unit] = Quantity[M-O#M, KG-O#KG, S-O#S, A-O#A, K-O#K, Mol-O#Mol, CD-O#CD]
    type Mul[O <: Unit] = Quantity[M+O#M, KG+O#KG, S+O#S, A+O#A, K+O#K, Mol+O#Mol, CD+O#CD]
  }

  type / [A <: Unit, B <: Unit] = A#Div[B]
  type ~ [A <: Unit, B <: Unit] = A#Mul[B] // We can not use * as a type, because it is reserved for variable length argument lists

  case class Quantity[M_ <: Exp, KG_ <: Exp, S_ <: Exp, A_ <: Exp, K_ <: Exp, Mol_ <: Exp, CD_ <: Exp](value: Double = 1.0) extends Unit {
    type M = M_
    type KG = KG_
    type S = S_
    type A = A_
    type K = K_
    type Mol = Mol_
    type CD = CD_

    type This = Quantity[M, KG, S, A, K, Mol, CD]
    def +(m : This) = Quantity[M, KG, S, A, K, Mol, CD](value + m.value)
    def -(m : This) = Quantity[M, KG, S, A, K, Mol, CD](value - m.value)
    def *[M2 <: Exp, KG2 <: Exp, S2 <: Exp, A2 <: Exp, K2 <: Exp, Mol2 <: Exp, CD2 <: Exp](m : Quantity[M2, KG2, S2, A2, K2, Mol2, CD2]) = Quantity[M + M2, KG + KG2, S + S2, A + A2, K + K2, Mol + Mol2, CD + CD2](value * m.value)
    def /[M2 <: Exp, KG2 <: Exp, S2 <: Exp, A2 <: Exp, K2 <: Exp, Mol2 <: Exp, CD2 <: Exp](m : Quantity[M2, KG2, S2, A2, K2, Mol2, CD2]) = Quantity[M - M2, KG - KG2, S - S2, A - A2, K - K2, Mol - Mol2, CD - CD2](value / m.value)

    def apply(v : Double) = Quantity[M, KG, S, A, K, Mol, CD](v * value)

    override def toString = "" + value
  }
  
  implicit def measure(v : Double) = Quantity[__, __, __, __, __, __, __](v)

  implicit def unitlessQuantityToDouble(q: Quantity[__, __, __, __, __, __, __]): Double = q.value

  // Unitless quantity
  type One                = Quantity[__, __, __, __, __, __, __];

  // Base units
  type Length             = Quantity[P1, __, __, __, __, __, __]; type m   = Length;            type metre    = m; type meter = metre
  type Mass               = Quantity[__, P1, __, __, __, __, __]; type kg  = Mass;              type kilogram = kg
  type Time               = Quantity[__, __, P1, __, __, __, __]; type s   = Time;              type second   = s
  type Current            = Quantity[__, __, __, P1, __, __, __]; type A   = Current;           type Ampere   = A
  type Temperature        = Quantity[__, __, __, __, P1, __, __]; type K   = Temperature;       type Kelvin   = K
  type AmountOfSubstance  = Quantity[__, __, __, __, __, P1, __]; type mol = AmountOfSubstance; type mole     = mol
  type LuminousIntensity  = Quantity[__, __, __, __, __, __, P1]; type cd  = LuminousIntensity; type candela  = cd

  // Shorthands for some exponents
  type s2 = s~s
  type s3 = s~s~s

  // Supplementary units
  type PlaneAngle           = One;   type rad = PlaneAngle; type radian = rad;
  type SolidAngle           = One;   type sr  = SolidAngle; type steradian = sr
  
  // Some derived units with symbols and alternative names
  type Frequency            = One/s;      type Hz  = Frequency;           type Hertz    = Hz

  type Force                = m~kg/s2;    type N   = Force;               type Newton   = N
  type Energy               = N~m;        type J   = Energy;              type Joule    = J
  type Power                = J/s;        type W   = Power;               type Watt     = W
  type Pressure             = N/m2;       type Pa  = Pressure;            type Pascal   = Pa

  type Voltage              = W/A;        type V   = Voltage;             type Volt     = V
  type ElectricCharge       = s~A;        type C   = ElectricCharge;      type Coulomb  = C
  type Capacitance          = C/V;        type F   = Capacitance;         type Farad    = F
  type ElectricResistance   = V/A;        type Ohm = ElectricResistance
  type Conductance          = A/V;        type S   = Conductance;         type Siemens  = S

  type MagneticFlux         = V~s;        type Wb  = MagneticFlux;        type Weber    = Wb
  type MagneticFluxDensity  = Wb/m2;      type T   = MagneticFluxDensity; type Tesla    = T
  type Inductance           = Wb/A;       type H   = Inductance;          type Henry    = H

  type LuminousFlux         = cd~sr;      type lm  = LuminousFlux;        type lumen    = lm
  type Illuminance          = lm/m2;      type lx  = Illuminance;         type lux      = lx

  type CatalyticActivity    = mol/s;      type kat = CatalyticActivity;   type katal    = kat

  type AbsorbedDose         = J/kg;       type Gy  = AbsorbedDose;        type Gray     = Gy
  type DoseEquivalent       = J/kg;       type Sv  = DoseEquivalent;      type Sievert  = Sv
  type IonizingRadiationActivity = One/s; type Bq  = IonizingRadiationActivity; type Becquerel = Bq

  // Derived units without own symbol
  type Area                 = m~m;        type m2  = Area
  type Volume               = m~m~m;      type m3  = Volume
  type Velocity             = m/s;        type Speed = Velocity
  type Acceleration         = m/s2
  type AngularVelocity      = rad/s
  type AngularAcceleration  = rad/s2

  type Density              = kg/m3;
  type SurfaceDensity       = kg/m2;
  type Concentration        = mol/m3
  type CatalyticActivityConcentration = kat/m3

  type DynamicViscosity     = Pa~s;
  type SurfaceTension       = N/m;
  type MomentOfForce        = N~m;

  type HeatFluxDensity      = W/m2
  type Irradiance           = W/m2
  type HeatCapacity         = J/K
  type SpecificHeatCapacity = J/kg~K
  type ThermalConductivity  = W/m~K
  type MolarHeatCapacity    = J/mol~K

  type SpecificEnergy       = J/kg
  type EnergyDensity        = J/m3
  type MolarEnergy          = J/mol

  type ElectricFieldStrength = V/m
  type ElectricChargeDensity = C/m3
  type ElectricFluxDensity   = C/m2
  type Permittivity          = F/m
  type Permeability          = H/m

  type IonizingRadiationExposure  = C/kg
  type AbsorbedDoseRate           = Gy/s
  type RadiantIntensity           = W/sr
  type Radiance                   = W/m2~sr



  // Prefixes
  val yotta = 1e24
  val zetta = 1e21
  val exa = 1e18
  val peta = 1e15
  val tera = 1e12
  val giga = 1e9
  val mega = 1e6
  val kilo = 1e3
  val hecto = 1e2
  val deca = 1e1

  val deci = 1e-1
  val centi = 1e-2
  val milli = 1e-3
  val micro = 1e-6
  val nano = 1e-9
  val pico = 1e-12
  val femto = 1e-15
  val atto = 1e-18
  val zepto = 1e-21
  val yocto = 1e-27

  
  // Unitless values (angles)
  val unitlessOne = new One()

  /** Unit of radians.  E.g. 2*Pi*radians = 360 degrees. */
  val rad   = new rad()
  val Trad  = rad(tera)
  val Grad  = rad(giga)
  val Mrad  = rad(mega)
  val krad  = rad(kilo)
  val mrad  = rad(milli)
  val urad  = rad(micro)
  val nrad  = rad(nano)
  val prad  = rad(pico)

  // Length
  /** meter (unit of length) */
  val m    = new m()
  val meter = m
  val Tm   = m(tera)
  val Gm   = m(giga)
  val Mm   = m(mega)
  val km   = m(kilo)
  val dm   = m(deci)
  val cm   = m(centi)
  val mm   = m(milli)
  val um   = m(micro)
  val nm   = m(nano)
  val pm   = m(pico)

  // Mass
  /** gram (0.01 of unit of mass) */
  val g    = new kg(1e-3)
  val gram = g
  val Tg   = g(tera)
  val Gg   = g(giga)
  val Mg   = g(mega)
  val kg   = g(kilo)
  val mg   = g(milli)
  val ug   = g(micro)
  val ng   = g(nano)
  val pg   = g(pico)

  // Time
  /** second (unit of time) */
  val s    = new s()
  val second = s
  val Ts   = s(tera)
  val Gs   = s(giga)
  val Ms   = s(mega)
  val ks   = s(kilo)
  val ms   = s(milli)
  val us   = s(micro)
  val ns   = s(nano)
  val ps   = s(pico)

  // Electric current
  /** Ampere (unit of electric current) */
  val A    = new A()
  val Ampere = A
  val TA   = A(tera)
  val GA   = A(giga)
  val MA   = A(mega)
  val kA   = A(kilo)
  val mA   = A(milli)
  val uA   = A(micro)
  val nA   = A(nano)
  val pA   = A(pico)

  // Temperature
  /** Kelvin (unit of temperature) */
  val K   = new K()
  val Kelvin = K
  val TK  = K(tera)
  val GK  = K(giga)
  val MK  = K(mega)
  val kK  = K(kilo)
  val mK  = K(milli)
  val uK  = K(micro)
  val nK  = K(nano)
  val pK  = K(pico)

  // Amount of substance
  /** mole (unit of amount of substance ) */
  val mol  = new mol()
  val Tmol = mol(tera)
  val Gmol = mol(giga)
  val Mmol = mol(mega)
  val kmol = mol(kilo)
  val mmol = mol(milli)
  val umol = mol(micro)
  val nmol = mol(nano)
  val pmol = mol(pico)

  // Luminous intensity
  /** Candela (unit of luminous intensity) */
  val cd  = new cd()
  val candela = cd
  val Tcd = cd(tera)
  val Gcd = cd(giga)
  val Mcd = cd(mega)
  val kcd = cd(kilo)
  val mcd = cd(milli)
  val ucd = cd(micro)
  val ncd = cd(nano)
  val pcd = cd(pico)

  // Derived units
  /** Volt (unit of electric potential difference) */
  val V   = new V()
  val Volt = V
  val TV   = V(tera)
  val GV   = V(giga)
  val MV   = V(mega)
  val kV   = V(kilo)
  val mV   = V(milli)
  val uV   = V(micro)
  val nV   = V(nano)
  val pV   = V(pico)

  /** Newton (unit of force) */
  val N   = new N()
  val Newton = N
  val TN  = N(tera)
  val GN  = N(giga)
  val MN  = N(mega)
  val kN  = N(kilo)
  val mN  = N(milli)
  val uN  = N(micro)
  val nN  = N(nano)
  val pN  = N(pico)

  /** Pascal (unit of pressure) */
  val Pa  = new Pa()
  val Pascal = Pa
  val TPa = Pa(tera)
  val GPa = Pa(giga)
  val MPa = Pa(mega)
  val kPa = Pa(kilo)
  val hPa = Pa(hecto)
  val mPa = Pa(milli)
  val uPa = Pa(micro)
  val nPa = Pa(nano)
  val pPa = Pa(pico)

  /** Hertz (unit of frequency) */
  val Hz  = new Hz()
  val Hertz = Hz
  val THz = Hz(tera)
  val GHz = Hz(giga)
  val MHz = Hz(mega)
  val kHz = Hz(kilo)
  val mHz = Hz(milli)
  val uHz = Hz(micro)
  val nHz = Hz(nano)
  val pHz = Hz(pico)

  /** Joule (unit of energy) */
  val J   = new J()
  val Joule = J
  val TJ  = J(tera)
  val GJ  = J(giga)
  val MJ  = J(mega)
  val kJ  = J(kilo)
  val mJ  = J(milli)
  val uJ  = J(micro)
  val nJ  = J(nano)
  val pJ  = J(pico)

  /** Watt (unit of power) */
  val W   = new W()
  val Watt = W
  val TW  = W(tera)
  val GW  = W(giga)
  val MW  = W(mega)
  val kW  = W(kilo)
  val mW  = W(milli)
  val uW  = W(micro)
  val nW  = W(nano)
  val pW  = W(pico)

  /** Coulomb (unit of electric charge) */
  val C   = new C()
  val Coulomb = C
  val TC  = C(tera)
  val GC  = C(giga)
  val MC  = C(mega)
  val kC  = C(kilo)
  val mC  = C(milli)
  val uC  = C(micro)
  val nC  = C(nano)
  val pC  = C(pico)

  /** Siemens (conductance) */
  val S   = new S()
  val Siemens = S
  val TS  = S(tera)
  val GS  = S(giga)
  val MS  = S(mega)
  val kS  = S(kilo)
  val mS  = S(milli)
  val uS  = S(micro)
  val nS  = S(nano)
  val pS  = S(pico)

  /** Weber (Magnetic flux) */
  val Wb   = new Wb()
  val Weber= Wb
  val TWb  = Wb(tera)
  val GWb  = Wb(giga)
  val MWb  = Wb(mega)
  val kWb  = Wb(kilo)
  val mWb  = Wb(milli)
  val uWb  = Wb(micro)
  val nWb  = Wb(nano)
  val pWb  = Wb(pico)

  /** Tesla (Magnetic flux density) */
  val T     = new T()
  val Tesla = T
  val TT  = T(tera)
  val GT  = T(giga)
  val MT  = T(mega)
  val kT  = T(kilo)
  val mT  = T(milli)
  val uT  = T(micro)
  val nT  = T(nano)
  val pT  = T(pico)

  /** Gray (Absorbed dose) */
  val Gy     = new Gy()
  val Gray = Gy
  val TGy  = Gy(tera)
  val GGy  = Gy(giga)
  val MGy  = Gy(mega)
  val kGy  = Gy(kilo)
  val mGy  = Gy(milli)
  val uGy  = Gy(micro)
  val nGy  = Gy(nano)
  val pGy  = Gy(pico)

  /** Sievert (Dose equivalent) */
  val Sv     = new Sv()
  val Sievert = Sv
  val TSv  = Sv(tera)
  val GSv  = Sv(giga)
  val MSv  = Sv(mega)
  val kSv  = Sv(kilo)
  val mSv  = Sv(milli)
  val uSv  = Sv(micro)
  val nSv  = Sv(nano)
  val pSv  = Sv(pico)

  /** Bequerel(Ionizing radiation activity) */
  val Bq     = new Bq()
  val Bequerel = Bq
  val TBq  = Bq(tera)
  val GBq  = Bq(giga)
  val MBq  = Bq(mega)
  val kBq  = Bq(kilo)
  val mBq  = Bq(milli)
  val uBq  = Bq(micro)
  val nBq  = Bq(nano)
  val pBq  = Bq(pico)

  /** Farad (unit of electric capacitance) */
  val F   = new F()
  val Farad = F
  val TF  = F(tera)
  val GF  = F(giga)
  val MF  = F(mega)
  val kF  = F(kilo)
  val mF  = F(milli)
  val uF  = F(micro)
  val nF  = F(nano)
  val pF  = F(pico)

  /** Ohm (unit of electric resistance) */
  val Ohm  = new Ohm()
  val TOhm = Ohm(tera)
  val GOhm = Ohm(giga)
  val MOhm = Ohm(mega)
  val kOhm = Ohm(kilo)
  val mOhm = Ohm(milli)
  val uOhm = Ohm(micro)
  val nOhm = Ohm(nano)
  val pOhm = Ohm(pico)

  /** Henry (unit of inductance) */
  val H    = new H()
  val Henry = H
  val TH   = H(tera)
  val GH   = H(giga)
  val MH   = H(mega)
  val kH   = H(kilo)
  val mH   = H(milli)
  val uH   = H(micro)
  val nH   = H(nano)
  val pH   = H(pico)

  /** Lux (unit of illuminance) */
  val lx  = new lx()
  val lux  = lx
  val Tlx = lux(tera)
  val Glx = lux(giga)
  val Mlx = lux(mega)
  val klx = lux(kilo)
  val mlx = lux(milli)
  val ulx = lux(micro)
  val nlx = lux(nano)
  val plx = lux(pico)

  /** Lumen - unit of luminous flux.  */
  val lm = new lm()
  val lumen = lm
  val Tlm = lm(tera)
  val Glm = lm(giga)
  val Mlm = lm(mega)
  val klm = lm(kilo)
  val mlm = lm(milli)
  val ulm = lm(micro)
  val nlm = lm(nano)
  val plm = lm(pico)

  /** Kat (unit of catalytic activity) */
  val kat  = new kat()
  val Tkat = kat(tera)
  val Gkat = kat(giga)
  val Mkat = kat(mega)
  val kkat = kat(kilo)
  val mkat = kat(milli)
  val ukat = kat(micro)
  val nkat = kat(nano)
  val pkat = kat(pico)

  // Exponents of base units
  val m2  = m*m
  val km2 = km*km

  val m3  = m*m*m
  val cm3 = cm*cm*cm

  val s2 = s*s

  val s3 = s*s*s

  // Non-SI units
  /** Circle constant - amount of radians in a full circle.  See http://tauday.com/ */
  val Tau = 2*Pi

  /** Unit of turns. E.g. 0.5*turns = 180 degrees */
  val turns = new rad(Tau)

  /** Unit of degrees. E.g. 90*degrees = 0.25 turns*/
  val deg   = new rad(Tau/360.0)

  private val zeroCelsiusInKelvin = 273.15
  /** The temperature of zero celsius in kelvins. */
  val zeroCelsius = new Temperature(zeroCelsiusInKelvin)

  /** Converts a temperature expressed in degrees Celsius to Kelvin.*/
  def fromCelsius(value: Double): Temperature = new Temperature(value + zeroCelsiusInKelvin)
  /** Converts a temperature (in Kelvin) to degrees Celsius.*/
  def toCelsius(temperature: Temperature): Double = temperature.value - zeroCelsiusInKelvin


  /** Hectare = Area of 100 m * 100 m.*/
  val ha  = m(100) * m(100)

  /** A metric ton = 1000 kg */
  val tonne  = kg(1000)

  /** Litre = Volume of 1 dm * 1 dm * 1 dm.*/
  val litre = dm*dm*dm
  val L = litre

  /** Minute (60 seconds)*/
  val min  = 60*s
  /** Hour (60 minutes)*/
  val h = min(60)
  /** Day (24 hours)*/
  val day  = h(24)
  /** Julian year, defined as 365.25 days. */
  val year = day(365.25)

  /** bar (non-standard unit of pressure) = 100 kPa.*/
  val bar  = Pa(1e5)
  /** millibar (non-standard unit of pressure) = 100 Pa.*/
  val mbar = hPa
  /** atmosphere (non-standard unit of pressure) = 1013.25 mbar.*/
  val atm  = mbar(1013.25)

  val ElectronVolt = J(1.60217733e-19)
  val eV = ElectronVolt

  /** One calorie, as defined by ISO 31-4 to equal 4.184 J.  Used commonly to measure energy in food.*/
  val cal = J(4.184)
  val Tcal = cal(tera)
  val Gcal = cal(giga)
  val Mcal = cal(mega)
  val kcal = cal(kilo)
  val mcal = cal(milli)
  val ucal = cal(micro) 
  val ncal = cal(nano)
  val pcal = cal(pico)

  // Imperial units
  // Providing alias for plural version of unit

  /** nautical mile (non standard unit of length) = 1852 meters.*/
  val nauticalMile = m(1852);
  val nauticalMiles = nauticalMile

  /** knot (non-standard unit of speed) = one nautical mile / hour.*/
  val knot = nauticalMile / h
  val knots = knot

  val inch = mm(25.4)
  val inches = inch

  val foot = inch(12)
  val feet = foot

  val yard = foot(3)
  val yards = yard

  val mile = m(1609.344)
  val miles = mile

  val gallon = litre(4.54609)
  val gallons = gallon

  val ounce = g(28.349523125)
  val ounces = ounce

  val pound = ounce(16)
  val pounds = pound
  val lb = pound



}

