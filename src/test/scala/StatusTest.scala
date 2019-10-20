import Commands.{Add, Commit, Init, Status}
import better.files.File
import org.scalatest.FunSuite

class StatusTest extends FunSuite{
  test("untracked - one file not tracked") {
    val dirTestPath = System.getProperty("user.dir") + "/../StatusTest";
    val dirTest = File (dirTestPath).createIfNotExists(true)
    Init.init(dirTest)

    File(dirTestPath + "/hello").createIfNotExists().overwrite("hello");

    val s = Status.untracked(dirTest)
    dirTest.delete()
    assert (s === "\t" + Console.RED + "hello" + "\n");
  }

  test("untracked - one file and on dir not tracked") {
    val dirTestPath = System.getProperty("user.dir") + "/../StatusTest";
    val dirTest = File (dirTestPath).createIfNotExists(true)
    Init.init(dirTest)

    File(dirTestPath + "/hello").createIfNotExists().overwrite("hello");
    File(dirTestPath + "/dirHello/dir").createIfNotExists(true)

    val s = Status.untracked(dirTest)
    dirTest.delete()
    assert (s === "\t" + Console.RED + "hello" + "\n" + "\t" + Console.RED + "dirHello/" + "\n");
  }

  test("untracked - no file to track") {
    val dirTestPath = System.getProperty("user.dir") + "/../StatusTest";
    val dirTest = File (dirTestPath).createIfNotExists(true)
    Init.init(dirTest)

    val s = Status.untracked(dirTest)
    dirTest.delete()
    assert (s === "");
  }

  test("unstagged - one file not stagged") {
    val dirTestPath = System.getProperty("user.dir") + "/../StatusTest";
    val dirTest = File (dirTestPath).createIfNotExists(true)
    Init.init(dirTest)

    val f = File(dirTestPath + "/hello").createIfNotExists().overwrite("hello");

    Add.add(Seq("hello"), dirTest)

    f.overwrite("HELLO")

    val s = Status.unstagged(dirTest)

    dirTest.delete()
    assert (s === "\t" + Console.RED + "modified: " + "hello" + "\n");
  }

  test("unstagged - no file to stagged") {
    val dirTestPath = System.getProperty("user.dir") + "/../StatusTest";
    val dirTest = File (dirTestPath).createIfNotExists(true)
    Init.init(dirTest)

    val f = File(dirTestPath + "/hello").createIfNotExists().overwrite("hello");

    Add.add(Seq("hello"), dirTest)

    val s = Status.unstagged(dirTest)

    dirTest.delete()
    assert (s === "");
  }

  test("uncommited - one file to commit, no commit yet") {
    val dirTestPath = System.getProperty("user.dir") + "/../StatusTest";
    val dirTest = File (dirTestPath).createIfNotExists(true)
    Init.init(dirTest)

    val f = File(dirTestPath + "/hello").createIfNotExists().overwrite("hello");
    val args = Seq("hello")

    Add.add(args, dirTest)

    val s = Status.uncommited(dirTest)

    dirTest.delete()
    assert (s === "\t" + Console.GREEN + "new file: " + "hello" + "\n");
  }

  test("uncommited - one dir to commit, no commit yet") {
    val dirTestPath = System.getProperty("user.dir") + "/../StatusTest";
    val dirTest = File (dirTestPath).createIfNotExists(true)
    Init.init(dirTest)

    val dir = File(dirTestPath + "/helloDir").createIfNotExists(true)
    File(dirTestPath + "/helloDir/hello").createIfNotExists().overwrite("hello")

    val args = Seq("helloDir")

    Add.add(args, dirTest)

    val s = Status.uncommited(dirTest)

    dirTest.delete()
    assert (s === "\t" + Console.GREEN + "new file: " + "helloDir/hello" + "\n");
  }

  test("uncommited - one file to commit with commit yet") {
    val dirTestPath = System.getProperty("user.dir") + "/../StatusTest";
    val dirTest = File (dirTestPath).createIfNotExists(true)
    Init.init(dirTest)

    val f = File(dirTestPath + "/hello").createIfNotExists().overwrite("hello");
    val args = Seq("hello")

    Add.add(args, dirTest)
    Commit.makeCommit("Premier commit", dirTest)

    f.overwrite("HELLO")

    Add.add(args, dirTest)

    val s = Status.uncommited(dirTest)

    dirTest.delete()
    assert (s === "\t" + Console.GREEN + "modified: " + "hello" + "\n");
  }
}
