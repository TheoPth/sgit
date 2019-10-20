package Commands

import Utils.JSON.{ORef, UJson, URef}
import Utils.MoveDir.Sdir
import Utils.archiSgit.UCommit
import better.files.File

object Branch {

  def branch(args: Seq[String], dirAct: File): Unit = {
    var repo: File = Sdir.getWD(dirAct)

    args match {
      case Seq(arg) if arg.equals("-av") => print(branchAv(repo))
      case Seq(nameBranch) => newBranch(nameBranch, repo)
      case _ => println("See usage")
    }
  }

  def newBranch(branchName: String, repo: File) : Unit = {
    // I
    val fRef = Sdir.getRef(repo)
    val oRef = UJson.readDeserializedJson[ORef](fRef)

    // Treatments
    val nRef = URef.addBranch(branchName, oRef)

    // O
    UJson.writeJson[ORef](nRef,fRef)
  }

  def branchAv(repo: File): String = {
    // I
    val fRef = Sdir.getRef(repo)
    val oRef = UJson.readDeserializedJson[ORef](fRef)
    val branchs = URef.getBranchs(oRef)
    val curName = URef.getCurrentBranchName(oRef)
    var res = ""

    branchs.map(branch => {
      val commit = Sdir.getCommitFromHash(branch.ref, repo)

      if (curName.equals(branch.name)) res += "*" else " "
      commit match {
        case Some(commit) => res += branch.name + " " + commit.hash.slice(0, 6) + " " + commit.message + "\n"
        case None => ""
      }
    })
    res
  }
}
