package Commands

import Commands.Branch.{branchAv, newBranch}
import Utils.JSON.{ORef, UJson, URef}
import Utils.MoveDir.Sdir
import better.files.File

object Tag {

  def tag(args: Seq[String], dirAct: File): Unit = {
    val repo: File = Sdir.getWD(dirAct)

    args match {
      case Seq(tagName) => newTag(tagName, repo)
      case _ => println("See Usage")
    }
  }

  def newTag(tagName: String, repo: File): Unit = {
    // I
    val fRef = Sdir.getRef(repo)
    val oRef = UJson.readDeserializedJson[ORef](fRef)

    // Treatments
    val nRef = URef.addTag(tagName, oRef)

    // O
    UJson.writeJson[ORef](nRef,fRef)
  }

}
