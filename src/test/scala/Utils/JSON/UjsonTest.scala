package Utils.JSON

import Commands.Init
import Utils.MoveDir.Sdir
import Utils.archiSgit.{OCommit, UCommit}
import better.files.File
import org.scalatest.FunSuite

class UjsonTest extends FunSuite {
  test("serialize/deserialize - should be the same object") {
    val commit = OCommit("prev", "mess", "me", "auj", Seq(""), "hash")
    val commmit2 = UJson.deserializeT[OCommit](UJson.serializeT[OCommit](commit))
    assert(commit == commmit2)

  }

  test("getCurrentBranchName - Retrieve name of current branch") {
    val dirTestPath = System.getProperty("user.dir") + "/../jsonTest";
    val dirTest = File (dirTestPath).createIfNotExists(true)
    Init.init(dirTest);
    val ref = UJson.readDeserializedJson[ORef]( Sdir.getRef(dirTest))
    assert(URef.getCurrentBranchName(ref) == "master");
    dirTest.delete()
  }

  test("getHashCurrentCommit - Retrieve hash of current commit") {
    val dirTestPath = System.getProperty("user.dir") + "/../jsonTest";
    val dirTest = File (dirTestPath).createIfNotExists(true)
    Init.init(dirTest);

    val ref = UJson.readDeserializedJson[ORef](Sdir.getRef(dirTest))
    val hCommit = URef.getHashCurrentCommit(ref)
    assert(hCommit == "")

    dirTest.delete()
  }

  test("changeCurrentCommit - Add new commit on a branch, should change the pointer") {
    val dirTestPath = System.getProperty("user.dir") + "/../jsonTest";
    val dirTest = File (dirTestPath).createIfNotExists(true)
    Init.init(dirTest)

    val fRef = Sdir.getRef(dirTest)

    val ref = UJson.readDeserializedJson[ORef]( Sdir.getRef(dirTest))
    val nref = URef.changeCurrentCommit("Lenouveauxhash", ref)

    val hCommit = URef.getHashCurrentCommit(nref)
    dirTest.delete()
    assert(hCommit == "Lenouveauxhash");
  }
}