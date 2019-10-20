package Commands

import Utils.Difference.DiffEnum._
import Utils.Difference.DifferenceFile
import org.scalatest.FunSuite

class LogTest extends FunSuite {
  test("maxSizeString - should return max") {
    val max = Log.maxSizeString(Seq("es", "popopo"))
    assert(max == 6)
  }

  test("fileToString - should print file stat in right way") {
    val d1 = DifferenceFile(ADD, 0, "a")
    assert(Log.fileToString("f1", 10, Seq(d1)) === " f1         | 1 " + Console.GREEN + "+" + Console.RED + Console.WHITE + "\n")
  }

  /*test("commitToLog - ") {
    val dirTestPath = System.getProperty("user.dir") + "/../commitToLog";
    val dirTest = File (dirTestPath).createIfNotExists(true)
    Init.init(dirTest);

    val f = File(dirTestPath + "/f1").createIfNotExists().overwrite("hello")
    val argsAdd = Seq("f1")
    Add.makeAdd(argsAdd, dirTest)
    Commit.makeCommit("First commit", dirTest)

    val headCommit = Sdir.getOptHeadCommitDir(dirTest)

    assert(headCommit.get == "")
  }*/

}
