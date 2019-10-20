package Utils.MoveDir

import java.nio.file.Path

import Utils.JSON.{ORef, UJson, URef}
import Utils.archiSgit.{OCommit, UCommit}
import better.files.File

object Sdir {
  val relativePathToBranchAct = "/branch/HEAD/"
  val relativePathToStageArea = "/SA"
  val relativePathToHead = "/head"
  val relativePathToDirCommits = "/commits"
  val relativePathToDirRefs = "/ref.json"

  def getWD(dirAct : File): File = {
    val pathToSgit: Path = MoveDir.findPathSgit(dirAct.path)
    File(pathToSgit + "/..")
  }

  def getSA(dirAct : File): File = {
    val pathToSgit: Path = MoveDir.findPathSgit(dirAct.path)
    File(pathToSgit + "/../.sgit" + relativePathToStageArea)
  }

  def getHead(dirAct : File): File = {
    // Branch is stocked in ref file
    val pathToSgit: Path = MoveDir.findPathSgit(dirAct.path)
    File(pathToSgit + relativePathToHead)
  }

  def getCommitDir(dirAct: File): File = {
    val pathToSgit: Path = MoveDir.findPathSgit(dirAct.path)
    File(pathToSgit + relativePathToDirCommits)
  }

  def getCommitFromHash(hash: String, repo: File): Option[OCommit] = {
    val pathToSgit: Path = MoveDir.findPathSgit(repo.path)
    val commitDir = File(pathToSgit + relativePathToDirCommits + "/" + hash)

    if(commitDir.exists) {
      Some(UCommit.getCommitJsonFromDir(commitDir))
    } else {
      None
    }
  }

  def getOptHeadCommitDir(dirAct: File): Option[File] = {
    // Retrieve the name of the last commit
    val oRef = UJson.readDeserializedJson[ORef](Sdir.getRef(dirAct))
    val nameCurrentCommit = URef.getHashCurrentCommit(oRef)
    if (nameCurrentCommit.equals("")) None else Some(File(getCommitDir(dirAct) + "/" +  nameCurrentCommit))
  }

  def getOptCommitDir(hash: String, repo: File): Option[File] = {
    // Retrieve the name of the last commit
    val pathToSgit: Path = MoveDir.findPathSgit(repo.path)
    val dir = File(getCommitDir(pathToSgit) + "/" +  hash)
    if (dir.exists) Some(dir) else None
  }

  def getOptHeadCommit(dirAct: File): Option[File] = {
    val oRef = UJson.readDeserializedJson[ORef](Sdir.getRef(dirAct))
    val nameCurrentCommit = URef.getHashCurrentCommit(oRef);
    if (nameCurrentCommit.equals("")) None else Some(File(getCommitDir(dirAct) + "/" +  nameCurrentCommit + "/src"))
  }

  def getRef(dirAct: File): File = {
    val pathToSgit: Path = MoveDir.findPathSgit(dirAct.path)
    File(pathToSgit + relativePathToDirRefs)
  }


  def getPrecCommits(c: File): Seq[File] = {
    val pathDir = c.parent.pathAsString
    def aux(commit: File, res: Seq[File]): Seq[File] = {
      val curCommit: OCommit = UCommit.getCommitJsonFromDir(commit)
      curCommit.prevCommit match {
        case "" => res
        case hash => {
          val newCommit = File(pathDir + "/" + hash)
          aux(newCommit, res :+ newCommit)
        }

      }
    }
    aux(c, Seq(c))
  }
}
