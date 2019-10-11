package Commands.Init

import java.nio.file.Paths

import Commands.Add
import Utils.Difference.{DiffEnum, DifferenceDir}
import better.files.File
import org.scalatest.FunSuite

import scala.math.Ordering.BooleanOrdering

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

    val files = Add.getRelativePathFromRegexOrNames(Seq("hello/hello.txt"), dirTest)

    dirTest.delete()

    assert(files.length == 1)
    assert(files === Seq(Paths.get("hello/hello.txt")))
  }

  test ("getRelativePathFromRegexOrNames - find several file with name in several directory") {
    val dirTestPath = System.getProperty("user.dir") + "/../TestgetFilesSelected";
    val dirTest = File (dirTestPath).createIfNotExists(true)
    File (dirTestPath + "/hello").createIfNotExists(true)
    File (dirTestPath + "/hello2").createIfNotExists(true)

    File (dirTestPath + "/hello/hello.txt").createIfNotExists().overwrite("hello")
    File (dirTestPath + "/hello2/hello2.txt").createIfNotExists().overwrite("hello")

    val files = Add.getRelativePathFromRegexOrNames(Seq("hello/hello.txt", "hello2/hello2.txt"), dirTest)

    dirTest.delete()

    assert(files.length == 2)
    assert(files === Seq(Paths.get("hello/hello.txt"), Paths.get("hello2/hello2.txt")))
  }

  test ("getRelativePathFromRegexOrNames - select dir from name ") {
    val dirTestPath = System.getProperty("user.dir") + "/../TestgetFilesSelected";
    val dirTest = File (dirTestPath).createIfNotExists(true)
    File (dirTestPath + "/hello").createIfNotExists(true)

    File (dirTestPath + "/hello/hello.txt").createIfNotExists().overwrite("hello")

    val files = Add.getRelativePathFromRegexOrNames(Seq("hello"), dirTest)

    dirTest.delete()

    assert(files.length == 1)
    assert(files === Seq(Paths.get("hello") ))
  }

  test("computeAllDiffAndConcat - one diff between file"){
    val dirTestPath = System.getProperty("user.dir") + "/../computeAllDiffAndConcatTest";
    val dirTest = File (dirTestPath).createIfNotExists(true)

    val dir1 = File (dirTestPath + "/dir1").createIfNotExists(true)
    val dir2 = File (dirTestPath + "/dir2").createIfNotExists(true)

    File (dirTestPath + "/dir1/hello.txt").createIfNotExists().overwrite("hello1")
    File (dirTestPath + "/dir2/hello.txt").createIfNotExists().overwrite("hello2")
    val diffs = Add.computeAllDiffAndConcat(Seq(Paths.get("hello.txt")), dir1, dir2)

    dirTest.delete()

    assert(Seq(DifferenceDir("hello.txt", DiffEnum.MODIFY)) == diffs)
  }

  test("computeAllDiffAndConcat - no diff between file"){
    val dirTestPath = System.getProperty("user.dir") + "/../computeAllDiffAndConcatTest";
    val dirTest = File (dirTestPath).createIfNotExists(true)

    val dir1 = File (dirTestPath + "/dir1").createIfNotExists(true)
    val dir2 = File (dirTestPath + "/dir2").createIfNotExists(true)

    File (dirTestPath + "/dir1/hello.txt").createIfNotExists().overwrite("hello1")
    File (dirTestPath + "/dir2/hello.txt").createIfNotExists().overwrite("hello1")
    val diffs = Add.computeAllDiffAndConcat(Seq(Paths.get("hello.txt")), dir1, dir2)

    dirTest.delete()

    assert(Seq() == diffs)
  }

  test("computeAllDiffAndConcat - One file in another dir"){
    val dirTestPath = System.getProperty("user.dir") + "/../computeAllDiffAndConcatTest";
    val dirTest = File (dirTestPath).createIfNotExists(true)

    val dir1 = File (dirTestPath + "/dir1").createIfNotExists(true)
    File (dirTestPath + "/dir1/dir1bis").createIfNotExists(true)
    val dir2 = File (dirTestPath + "/dir2").createIfNotExists(true)

    File (dirTestPath + "/dir1/hello.txt").createIfNotExists().overwrite("hello1")
    File (dirTestPath + "/dir1/dir1bis/hellooo.txt").createIfNotExists().overwrite("hello1")
    File (dirTestPath + "/dir2/hello.txt").createIfNotExists().overwrite("hello1")

    val diffs = Add.computeAllDiffAndConcat(Seq(Paths.get("dir1bis/helloo.txt")), dir1, dir2)

    dirTest.delete()

    assert(Seq(DifferenceDir( "dir1bis/helloo.txt", DiffEnum.DELETE)) == diffs)
  }

  test("Add - One file created") {
    val dirTestPath = System.getProperty("user.dir") + "/../addTest";
    val dirTest = File (dirTestPath).createIfNotExists(true)
    Init.init(dirTest);

    File(dirTestPath + "/hello.txt").createIfNotExists().overwrite("hello");

    val args = Seq("hello.txt")
    Add.add(args, dirTest);

    assert(File(dirTestPath + "/.sgit/SA/hello.txt").exists == true)

    dirTest.delete();
  }

  test("Add - One file created in two directories deep") {
    val dirTestPath = System.getProperty("user.dir") + "/../addTest";
    val dirTest = File (dirTestPath).createIfNotExists(true)
    Init.init(dirTest);

    File(dirTestPath + "/dir1/dir2").createIfNotExists(true, true)
    File(dirTestPath + "/dir1/dir2/hello.txt").createIfNotExists().overwrite("hello");

    val args = Seq("dir1/dir2/hello.txt")
    Add.add(args, dirTest);

    val assertBool: Boolean = File(dirTestPath + "/.sgit/SA/dir1/dir2/hello.txt").exists == true

    dirTest.delete();

    assert(assertBool)
  }

  test("Add - One file in two directories deep, current directory inside the deep") {
    val dirTestPath = System.getProperty("user.dir") + "/../addTest";
    val dirTest = File (dirTestPath).createIfNotExists(true)
    Init.init(dirTest);

    File(dirTestPath + "/dir1/dir2").createIfNotExists(true, true)
    File(dirTestPath + "/dir1/dir2/hello.txt").createIfNotExists().overwrite("hello");

    val args = Seq("hello.txt")
    val dirAct = File(dirTestPath + "/dir1/dir2")
    Add.add(args, dirAct);

    val assertBool: Boolean = File(dirTestPath + "/.sgit/SA/dir1/dir2/hello.txt").exists == true

    dirTest.delete();

    assert(assertBool)
  }

  test("Add - One file modified") {
    val dirTestPath = System.getProperty("user.dir") + "/../addTestDeleted";
    val dirTest = File (dirTestPath).createIfNotExists(true)
    Init.init(dirTest);

    val fileCreated = File(dirTestPath + "/hello.txt").createIfNotExists().overwrite("hello");

    val args = Seq("hello.txt")

    Add.add(args, dirTest);

    fileCreated.overwrite("New hello");

    Add.add(args, dirTest);

    val assertBool: Boolean = File(dirTestPath + "/.sgit/SA/hello.txt").isSameContentAs(fileCreated)

    dirTest.delete()

    assert(assertBool)
  }

  test("Add - One file deleted") {
    val dirTestPath = System.getProperty("user.dir") + "/../addTestOneFileDeleted";
    val dirTest = File (dirTestPath).createIfNotExists(true)
    Init.init(dirTest);

    var file = File(dirTestPath + "/hello.txt").createIfNotExists().overwrite("hello");

    val args = Seq("hello.txt")

    Add.add(args, dirTest);

    file.delete()

    Add.add(args, dirTest);

    val fileExist: Boolean = File(dirTestPath + "/.sgit/SA/hello.txt").exists

    dirTest.delete()

    assert(!fileExist)
  }
}
