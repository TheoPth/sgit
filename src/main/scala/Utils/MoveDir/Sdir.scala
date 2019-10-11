package Utils.MoveDir

import java.nio.file.Path

import Utils.JSON.{UJson, URef}
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

  def getHeadCommit(dirAct: File): Option[File] = {
    // Retrieve the name of the last commit
    val nameCurrentCommit = URef.getHashCurrentCommit(getRef(dirAct));

    if (nameCurrentCommit.equals("")) None else Some(File(getCommitDir(dirAct) + "/" +  nameCurrentCommit))
  }

  def getRef(dirAct: File): File = {
    val pathToSgit: Path = MoveDir.findPathSgit(dirAct.path)
    File(pathToSgit + relativePathToDirRefs)
  }
}
