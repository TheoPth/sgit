package Utils.MoveDir

import java.nio.file.{Path, Paths}

import Commands.Init.Init
import better.files.File
import org.scalatest.FunSuite

class MoveDirTest extends FunSuite{

  test("containsFileWithName - True") {
    val dirTestPath = System.getProperty("user.dir") + "/../test/";
    val dir = File(dirTestPath).createIfNotExists(true)

    File( dir + "/find.txt").createIfNotExists().overwrite("hello")

    val bool = MoveDir.containsFileWithName(dir.path, "find.txt")
    dir.delete()

    assert(bool == true)
  }

  test("containsFileWithName - False") {
    val dirTestPath = System.getProperty("user.dir") + "/../test/";
    val dir = File(dirTestPath).createIfNotExists(true)

    File( dir + "/find.txt").createIfNotExists().overwrite("hello")

    val bool = MoveDir.containsFileWithName(dir.path, "finded.txt")
    dir.delete()

    assert(bool == false)
  }

  test("containsFileWithName - False with no file") {
    val dirTestPath = System.getProperty("user.dir") + "/../test/";
    val dir = File(dirTestPath).createIfNotExists(true)

    val bool = MoveDir.containsFileWithName(dir.path, "finded.txt")
    dir.delete()

    assert(bool == false)
  }

  test("findPathSgit - .sgit in the same directory") {
    val dirTestPath = System.getProperty("user.dir") + "/../test/";
    var dir = File(dirTestPath).createIfNotExists(true)
    Init.init(dir.pathAsString);
    val path = MoveDir.findPathSgit(dir.path)
    dir.delete()

    val pathFileExpected = File(dirTestPath + "/.sgit").path
    assert(path === pathFileExpected)
  }

  test("findPathSgit - .sgit in the parent directory") {
    val dirTestPath = System.getProperty("user.dir") + "/../test/";
    val dir = File(dirTestPath).createIfNotExists(true)
    Init.init(dir.pathAsString);

    // Create the directory start
    val dirStart = File(dirTestPath + "/dir1/dir2").createIfNotExists(true, true)
    val path = MoveDir.findPathSgit(dirStart.path)

    dir.delete()
    val pathFileExpected = File(dirTestPath + "/.sgit").path
    assert(path == pathFileExpected)
  }

  test("findPathSgit - no .sgit should return null") {
    val dirTestPath = System.getProperty("user.dir") + "/../test/";
    val dir = File(dirTestPath).createIfNotExists(true)

    // Create the directory start
    val dirStart = File(dirTestPath + "/dir1").createIfNotExists(true, true)
    val path = MoveDir.findPathSgit(dirStart.path)

    dir.delete()
    assert(path == null)
  }

  test("findRelativePathSgit - .sgit in the same directory") {
    val dirTestPath = System.getProperty("user.dir") + "/../test/";
    val dir = File(dirTestPath).createIfNotExists(true)
    Init.init(dir.pathAsString);

    val path = MoveDir.findRelativePathSgit(dir.path)
    dir.delete()

    assert(path.toString === ".sgit")
  }

  test("findRelativePathSgit - .sgit in the parent directory") {
    val dirTestPath = System.getProperty("user.dir") + "/../test/";
    val dir = File(dirTestPath).createIfNotExists(true)
    Init.init(dir.pathAsString);

    // Create the directory start
    val dirStart = File(dirTestPath + "/dir1/dir2").createIfNotExists(true, true)
    val path = MoveDir.findRelativePathSgit(dirStart.path)

    dir.delete()
    assert(path.toString === "../../.sgit")
  }

  test("findRelativePathSgit - no .sgit should return null") {
    val dirTestPath = System.getProperty("user.dir") + "/../test/";
    val dir = File(dirTestPath).createIfNotExists(true)

    // Create the directory start
    val dirStart = File(dirTestPath + "/dir1").createIfNotExists(true, true)
    val path = MoveDir.findRelativePathSgit(dirStart.path)

    dir.delete()
    assert(path == null)
  }
}
