package Utils.archiSgit

import Utils.JSON.{ORef, UJson, URef}
import Utils.MoveDir.Sdir
import Utils.MoveDir.Sdir.getRef
import better.files.File

case class OCommit(prevCommit: String, message: String, author: String, date: String, tags: Seq[String], hash: String)

object UCommit {
  def commitToString(dirCommit: File): String = {
    val infCommit = getCommitJsonFromDir(dirCommit)
    raw"""${Console.YELLOW}commit ${infCommit.hash}${Console.WHITE}
      |Author: ${infCommit.author}
      |Date: ${infCommit.date}
      |
      |       ${infCommit.message}
      |""".stripMargin
  }

  // Serialize and save commit in a file
  def saveCommit(commit: OCommit, file: File): Unit = {
    val ser = UJson.serializeT(commit)
    file.overwrite(ser)
  }

  // return the commit json
  // Args : Commit - dir of the commit
  def getCommitJsonFromDir(commitDir: File) : OCommit = {
    val fCommit = File(commitDir.pathAsString + "/commit.json")
    UJson.readDeserializedJson[OCommit](fCommit)
  }

  def getHashCommitAct(dirCommit: File): String = {
    val oRef = UJson.readDeserializedJson[ORef](dirCommit)
    URef.getHashCurrentCommit(oRef)
  }

  def getCurrentCommit(dirAct: File): Option[OCommit] = {
    val optCommitDir = Sdir.getOptHeadCommitDir(dirAct)

    optCommitDir match {
      case Some(commitDir) => Some(getCommitJsonFromDir(commitDir))
      case None => None
    }
  }

  def getAllCommits(repo: File): Seq[OCommit] = {
    Sdir.getCommitDir(repo).children.toSeq.map(child => {
      getCommitJsonFromDir(child)
    })
  }


  def getAllCommitsPrec(commit: OCommit) : Seq[OCommit] = {
    val pathDir = commit.prevCommit

    def aux(commit: OCommit, commits: Seq[OCommit]): Seq[OCommit] = {
      commit.prevCommit match {
        case "" => commits
        case hash => {
          val newCommit = getCommitJsonFromDir(File(pathDir + "/" + hash))
          aux(newCommit, commits :+ newCommit)
        }
      }
    }
    aux(commit, Seq(commit))
  }
}
