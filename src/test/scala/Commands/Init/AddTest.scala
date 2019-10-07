package Commands.Init

import java.nio.file.{Paths}

import Commands.Add
import better.files.File
import org.scalatest.FunSuite

class AddTest extends FunSuite {
  test ("getRelativePathFromRegexOrNames - find one file with regex") {
    val dirTestPath = System.getProperty("user.dir") + "/../TestgetFilesSelected";
    val dirTest = File (dirTestPath).createIfNotExists(true)
    File (dirTestPath + "/hello.txt").createIfNotExists().overwrite("hello")

    val files = Add.getRelativePathFromRegexOrNames(Seq("*.txt"), dirTest)

    dirTest.delete()

    assert(files.length == 1)
    assert(files.head === Paths.get("hello.txt"))
  }

  test ("getRelativePathFromRegexOrNames - find one file with name") {
    val dirTestPath = System.getProperty("user.dir") + "/../TestgetFilesSelected";
    val dirTest = File (dirTestPath).createIfNotExists(true)
    File (dirTestPath + "/hello.txt").createIfNotExists().overwrite("hello")

    val files = Add.getRelativePathFromRegexOrNames(Seq("hello.txt"), dirTest)

    dirTest.delete()

    assert(files.length == 1)
    assert(files.head === Paths.get("hello.txt"))
  }

  test ("getRelativePathFromRegexOrNames - find multiple file with regex") {
    val dirTestPath = System.getProperty("user.dir") + "/../TestgetFilesSelected";
    val dirTest = File (dirTestPath).createIfNotExists(true)
    File (dirTestPath + "/hello.txt").createIfNotExists().overwrite("hello")
    File (dirTestPath + "/hello2.txt").createIfNotExists().overwrite("hello")

    val files = Add.getRelativePathFromRegexOrNames(Seq("*.txt"), dirTest)

    dirTest.delete()

    assert(files.length == 2)
    assert(files === Seq(Paths.get("hello2.txt"), Paths.get("hello.txt")))
  }

  test ("getRelativePathFromRegexOrNames - find multiple file with name") {
    val dirTestPath = System.getProperty("user.dir") + "/../TestgetFilesSelected";
    val dirTest = File (dirTestPath).createIfNotExists(true)
    File (dirTestPath + "/hello.txt").createIfNotExists().overwrite("hello")
    File (dirTestPath + "/hello2.txt").createIfNotExists().overwrite("hello")

    val files = Add.getRelativePathFromRegexOrNames(Seq("hello.txt", "hello2.txt"), dirTest)

    dirTest.delete()

    assert(files.length == 2)
    assert(files === Seq(Paths.get("hello.txt"), Paths.get("hello2.txt")))
  }

  test ("getRelativePathFromRegexOrNames - find one file with name in directory") {
    val dirTestPath = System.getProperty("user.dir") + "/../TestgetFilesSelected";
    val dirTest = File (dirTestPath).createIfNotExists(true)
    File (dirTestPath + "/hello").createIfNotExists(true)


    File (dirTestPath + "/hello/hello.txt").createIfNotExists().overwrite("hello")

    val files = Add.getRelativePathFromRegexOrNames(Seq("hello/*.txt"), dirTest)

    dirTest.delete()

    assert(files.length == 1)
    assert(files === Seq(Paths.get("hello/hello.txt")))
  }

}
