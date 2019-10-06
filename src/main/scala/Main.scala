import better.files._


object Main extends App {
  val dir = System.getProperty("user.dir")

  /*val p = File(dir).path
  val q = File(dir + "/../dos2").path
  //println(p)
  //println(q)
  val relativePath =  q.relativize(p)
  val childs = File(dir).children.toSeq
  println(childs.head)
  println(childs.tail.head)*/

  val p = File(dir + "/dos1").children.toSeq
  val q = File(dir + "/dos2").children.toSeq
  val isContained = q.contains(p.head)
  println(p.head.path)
  println(q.head.path)
  println(isContained)

}



