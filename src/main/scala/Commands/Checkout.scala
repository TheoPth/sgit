package Commands

import Commands.Branch.{branchAv, newBranch}
import Utils.JSON.{OField, ORef, UJson, URef}
import Utils.MoveDir.Sdir
import Utils.archiSgit.{OCommit, UCommit}
import better.files.File

object Checkout {

  def checkout(args: Seq[String], dirAct: File): Unit = {
    val repo: File = Sdir.getWD(dirAct)

    args match {
      case Seq(target) => switchTo(target, repo)
      case _ => println("See usage")
    }
  }

  def switchTo(target: String, repo: File): String = {
    // I
    val fRef = Sdir.getRef(repo)
    val oRef = UJson.readDeserializedJson[ORef](fRef)
    val branchs = URef.getBranchs(oRef)
    val tags = URef.getTags(oRef)
    val commits = UCommit.getAllCommits(repo)

    // treatements
    val commit = findCommit(branchs, tags, commits,target)

    // O

    commit match  {
      case Some(commit) => {
        moveToCommit(commit, repo)
        "Switched to " + target
      }
      case None => "error: pathspec '" + target + "' did not match any file(s) known to sgit."
    }
  }

  def findCommit(branchs: Seq[OField], tags: Seq[OField], commits: Seq[OCommit], arg: String) : Option[OCommit] = {
    var hashCommit = ""

    val branch = branchs.filter(branch => branch.name == arg)
    if (branch.length > 0) hashCommit = branch(0).name

    val tag = tags.filter(tag => tag.name == arg)
    if (tag.length > 0) hashCommit = tag(0).name

    val hCommit = commits.filter(commit => commit.hash.startsWith(arg))
    if (hCommit.length > 0) hashCommit = hCommit(0).hash

    // retrieve the right commit
    val commit = commits.filter(commit => commit.hash.startsWith(hashCommit))

    if (commit.length > 0) Some(commit(0)) else None
  }

  def moveToCommit(commit: OCommit, repo: File): Unit = {
    val commitDir = Sdir.getOptCommitDir(commit.hash, repo)
    val WD = Sdir.getWD(repo)

    // move files
    commitDir match {
      case Some(dir) => {
        WD.children.toSeq.filter(child => child.name != ".sgit").map(child => child.delete())
        dir.copyToDirectory(WD)
      }
      case None =>
    }

    // Move pointer

    // NO TIME 

  }
}
