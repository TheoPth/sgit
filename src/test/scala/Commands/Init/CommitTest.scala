package Commands.Init

import Commands.Add
import Commands.Commit.Commit
import Utils.JSON.URef
import Utils.MoveDir.Sdir
import better.files.File
import org.scalatest.FunSuite

class CommitTest extends FunSuite {
  test("createNewCommit") {
    val nc = Commit.createNewCommit("The hash", "The prev hash", "the message")

    assert(nc === """{
                   |  "prevCommit" : The prev hash,
                   |  "messsage" : the message,
                   |  "author": "",
                   |  "Date": "",
                   |  "tag": [],
                   |  "hash": The hash
                   |  }
                   |""".stripMargin)
  }

  test("Commit - one File two times")  {
    val dirTestPath = System.getProperty("user.dir") + "/../commitTest";
    val dirTest = File (dirTestPath)
    if (dirTest.exists) dirTest.delete()
    dirTest.createIfNotExists(true)
    Init.init(dirTest);

    File(dirTestPath + "/hello.txt").createIfNotExists().overwrite("Hello.txt")

    var args = Seq("hello.txt")
    Add.add(args, dirTest)

    var log = Commit.makeCommit("Premier commit", dirTest)
    assert(log.substring(16) === "Premier commit\n1 file changed, 1 insertions(+), 0 deletions(-)");

    val commit: File = Sdir.getCommitDir(dirTest).children.toSeq.head
    val nameLastCommit = URef.getHashCurrentCommit(Sdir.getRef(dirTest))
    assert(commit.name === nameLastCommit)

    val errorMessage = Commit.makeCommit("Deuxieme commit sans add", dirTest)
    assert(errorMessage === "nothing to commit, working tree clean")


    File(dirTestPath + "/hello2.txt").createIfNotExists().overwrite("Hello.txt")

    args = Seq("hello2.txt")
    Add.add(args, dirTest)
    Commit.makeCommit("Deuxième commit", dirTest)
    val commit2: File = Sdir.getCommitDir(dirTest).children.toSeq.head
    val nameLastCommit2 = URef.getHashCurrentCommit(Sdir.getRef(dirTest))
    assert(commit2.name === nameLastCommit2)

    dirTest.delete()
  }

  test("Commit - one file in one directory")  {
    val dirTestPath = System.getProperty("user.dir") + "/../commitTestOneFile";
    val dirTest = File (dirTestPath).createIfNotExists(true)
    Init.init(dirTest);

    File(dirTestPath + "/src").createIfNotExists(true)
    File(dirTestPath + "/src/hello.txt").createIfNotExists().overwrite("Hello.txt")

    val args = Seq("src/hello.txt")
    Add.add(args, dirTest)

    val log = Commit.makeCommit("Premier commit", dirTest)
    assert(log.substring(16) === "Premier commit\n1 file changed, 1 insertions(+), 0 deletions(-)");

    dirTest.delete()
  }

  test("Commit - one empty directory")  {
    val dirTestPath = System.getProperty("user.dir") + "/../commitTest";
    val dirTest = File (dirTestPath).createIfNotExists(true)
    Init.init(dirTest);

    File(dirTestPath + "/src").createIfNotExists(true)

    val args = Seq()
    Add.add(args, dirTest)

    val log = Commit.makeCommit("Premier commit", dirTest)
    assert(log === "nothing to commit, working tree clean");

    dirTest.delete()
  }
}
