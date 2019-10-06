package Utils.JSON

import org.json4s._
import org.json4s.native.JsonMethods._

object json {
  implicit val formats = DefaultFormats
  val dir = System.getProperty("user.dir")
  val json = parse(""" { "numbers" : [1, 2, 3, 4], "strings" : ["Emma"] } """)
  var jsonNum = (json \ "strings")(0)
  var calc = jsonNum.extract[String]
  println(calc)
}
