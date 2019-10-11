package Commands.Init

import better.files.File
import org.scalatest.FunSuite

class InitTest extends FunSuite{
  test("Create architecture") {
    // Create a dir to test
    val dirTestPath = System.getProperty("user.dir") + "/../testInit";
    val dirTest = File(dirTestPath).createIfNotExists(true)

    assert(Init.init(dirTest) == true)
    assert(dirTest.isEmpty == false)

    // Delete test
    dirTest.delete()
  }

  test("Don't scratch existing .sgit") {
    val dirTestPath = System.getProperty("user.dir") + "/../testInit";
    val dirTest = File(dirTestPath).createIfNotExists(true)

    Init.init(dirTest)
    assert(Init.init(dirTest) == false)

    // Delete test
    dirTest.delete()
  }
}
