package scalaquantity

object Subtractables {
  trait Subtractable {
    type SubType <: Subtractable
    type Sub[T <: SubType] <: SubType
  }

  type -[S1 <: Subtractable, S2 <: S1#SubType] = S1#Sub[S2]
}
