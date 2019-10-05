import better.files._
import File._
import java.io.{File => JFile}
import Commands.Init._

object Main extends App {
  val dir = System.getProperty("user.dir");
  Init.init(dir);
}
