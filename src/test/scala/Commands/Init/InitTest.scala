package Commands.Init

import better.files.File
import org.scalatest.FunSuite

class InitTest extends FunSuite{
  test("Create architecture") {
    // Create a dir to test
    val dir = System.getProperty("user.dir") + "/../";

    Init.init(dir)

    assert(File(dir + "/.sgit").isEmpty == false)

    // Delete test
    File(dir + "/.sgit").delete()
    assert(File(dir + "/.sgit").isEmpty == true)
  }

  test("Don't scratch existing .sgit") {
    val dir = System.getProperty("user.dir") + "/../";
    Init.init(dir)
    File(dir + "/.sgit/SA.json").renameTo("change")
    assert (File(dir + "/.sgit/change").isEmpty == false)
    assert (File(dir + "/.sgit/SA.json").isEmpty == true)
    Init.init(dir)
    assert (File(dir + "/.sgit/SA.json").isEmpty == true)

    // Delete test
    File(dir + "/.sgit").delete()
    assert(File(dir + "/.sgit").isEmpty == true)
  }
}
