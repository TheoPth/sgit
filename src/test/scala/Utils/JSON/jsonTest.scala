package Utils.JSON

import Utils.Difference.{DiffEnum, DifferenceDir}
import better.files.File
import org.scalatest.FunSuite

class jsonTest extends FunSuite {
  test("serializeSeqDifferenceDir - No diff in seq") {
    val diffs = Seq()
    val ser = Json.serializeSeqDifferenceDir(diffs)
    val res = Json.deserializeSeqDifferenceDir(ser)

    assert(res === diffs)
  }

  test("serializeSeqDifferenceDir - One in seq") {
    val diffs = Seq(DifferenceDir("hello.txt", DiffEnum.ADD));
    val ser = Json.serializeSeqDifferenceDir(diffs)
    val res = Json.deserializeSeqDifferenceDir(ser)

    assert(res === diffs)
  }


  test("write/read SerializeSeqDifferenceDir - One in seq") {
    val dirTestPath = System.getProperty("user.dir") + "/../jsonTest";
    val dirTest = File (dirTestPath).createIfNotExists(true)

    val jsonFile = File(dirTestPath + "/save.json").createIfNotExists()

    val diffs = Seq(DifferenceDir("hello.txt", DiffEnum.ADD));
    Json.writeSerializeSeqDifferenceDir(diffs, jsonFile)
    val res = Json.readDeserializeSeqDifferenceDir(jsonFile)

    assert(res === diffs)
    dirTest.delete()
  }

  test("write/read SerializeSeqDifferenceDir - no diff") {
    val dirTestPath = System.getProperty("user.dir") + "/../jsonTest";
    val dirTest = File (dirTestPath).createIfNotExists(true)

    val jsonFile = File(dirTestPath + "/save.json").createIfNotExists()

    val diffs = Seq();
    Json.writeSerializeSeqDifferenceDir(diffs, jsonFile)
    val res = Json.readDeserializeSeqDifferenceDir(jsonFile)

    assert(res === diffs)
    dirTest.delete()
  }

  test("write/read SerializeSeqDifferenceDir - several diffs") {
    val dirTestPath = System.getProperty("user.dir") + "/../jsonTest";
    val dirTest = File (dirTestPath).createIfNotExists(true)

    val jsonFile = File(dirTestPath + "/save.json").createIfNotExists()

    val diffs = Seq(DifferenceDir("hello.txt", DiffEnum.ADD),
      DifferenceDir("hello.txt", DiffEnum.DELETE),
      DifferenceDir("hello.txt", DiffEnum.MODIFY));
    Json.writeSerializeSeqDifferenceDir(diffs, jsonFile)
    val res = Json.readDeserializeSeqDifferenceDir(jsonFile)

    assert(res === diffs)

    dirTest.delete()
  }

}