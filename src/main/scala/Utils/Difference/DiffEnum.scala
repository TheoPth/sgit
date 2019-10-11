package Utils.Difference

object DiffEnum extends Enumeration with Serializable {
  val ADD = Value("ADD")
  val DELETE = Value("DELETE")
  val MODIFY = Value("MODIFY")
}
