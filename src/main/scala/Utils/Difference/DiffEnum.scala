package Utils.Difference

object DiffEnum extends Enumeration with Serializable {
  type DiffEnum = Value

  val ADD = Value
  val DELETE = Value
  val MODIFY = Value
}
