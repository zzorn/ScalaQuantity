package scalaquantity

object Addables {
  trait Addable {
    type AddType <: Addable
    type Add[T <: AddType] <: AddType
  }

  type +[A1 <: Addable, A2 <: A1#AddType] = A1#Add[A2]
}