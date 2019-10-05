package Commands.Init

import better.files.File
import org.scalatest.FunSuite

class InitTest extends FunSuite{
  test("Create dir") {
    // Create a dir to test
    val dir = System.getProperty("user.dir");
    println(dir)
    File(dir).createIfNotExists(true)

    Init.init(dir)

    assert(File(dir + "/.sgit").isEmpty == false)

    File(dir + "/.sgit").delete()
  }
}
