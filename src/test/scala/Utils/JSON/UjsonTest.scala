package Utils.JSON

import Commands.Init.Init
import Utils.Difference.{DiffEnum, DifferenceDir}
import Utils.MoveDir.Sdir
import better.files.File
import org.scalatest.FunSuite

class UjsonTest extends FunSuite {
  test("serializeSeqDifferenceDir - No diff in seq") {
    val diffs = Seq()
    val ser = UJson.serializeSeqDifferenceDir(diffs)
    val res = UJson.deserializeSeqDifferenceDir(ser)

    assert(res === diffs)
  }

  test("serializeSeqDifferenceDir - One in seq") {
    val diffs = Seq(DifferenceDir("hello.txt", DiffEnum.ADD));
    val ser = UJson.serializeSeqDifferenceDir(diffs)
    val res = UJson.deserializeSeqDifferenceDir(ser)

    assert(res === diffs)
  }


  test("write/read SerializeSeqDifferenceDir - One in seq") {
    val dirTestPath = System.getProperty("user.dir") + "/../jsonTest";
    val dirTest = File (dirTestPath).createIfNotExists(true)

    val jsonFile = File(dirTestPath + "/save.json").createIfNotExists()

    val diffs = Seq(DifferenceDir("hello.txt", DiffEnum.ADD));
    UJson.writeSerializeSeqDifferenceDir(diffs, jsonFile)
    val res = UJson.readDeserializeSeqDifferenceDir(jsonFile)

    assert(res === diffs)
    dirTest.delete()
  }

  test("write/read SerializeSeqDifferenceDir - no diff") {
    val dirTestPath = System.getProperty("user.dir") + "/../jsonTest";
    val dirTest = File (dirTestPath).createIfNotExists(true)

    val jsonFile = File(dirTestPath + "/save.json").createIfNotExists()

    val diffs = Seq();
    UJson.writeSerializeSeqDifferenceDir(diffs, jsonFile)
    val res = UJson.readDeserializeSeqDifferenceDir(jsonFile)

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
    UJson.writeSerializeSeqDifferenceDir(diffs, jsonFile)
    val res = UJson.readDeserializeSeqDifferenceDir(jsonFile)

    assert(res === diffs)

    dirTest.delete()
  }

  test("getCurrentBranchName - Retrieve name of current branch") {
    val dirTestPath = System.getProperty("user.dir") + "/../jsonTest";
    val dirTest = File (dirTestPath).createIfNotExists(true)
    Init.init(dirTest);

    assert(URef.getCurrentBranchName(Sdir.getRef(dirTest)) == "master");
    dirTest.delete()
  }

  test("getHashCurrentCommit - Retrieve hash of current commit") {
    val dirTestPath = System.getProperty("user.dir") + "/../jsonTest";
    val dirTest = File (dirTestPath).createIfNotExists(true)
    Init.init(dirTest);
    val fRef = Sdir.getRef(dirTest)

    val hCommit = URef.getHashCurrentCommit(fRef)
    assert(hCommit == "")

    dirTest.delete()
  }

  test("changeCurrentCommit - Add new commit on a branch, should change the pointer") {
    val dirTestPath = System.getProperty("user.dir") + "/../jsonTest";
    val dirTest = File (dirTestPath).createIfNotExists(true)
    Init.init(dirTest);
    val fRef = Sdir.getRef(dirTest)

    URef.changeCurrentCommit("Lenouveauxhash", fRef)
    val hCommit = URef.getHashCurrentCommit(fRef)
    dirTest.delete()
    assert(hCommit == "Lenouveauxhash");
  }

}