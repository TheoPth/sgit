package Commands.Commit

import Utils.Difference.{DiffEnum, Difference, DifferenceDir}
import Utils.JSON.{UJson, URef}
import Utils.MoveDir.{MoveDir, Sdir}
import better.files.File
import com.roundeights.hasher.Implicits._
import org.json4s.JsonAST.{JField, JString}
import org.json4s.NoTypeHints
import org.json4s.native.Serialization

import scala.language.postfixOps


object Commit {
  implicit val formats = Serialization.formats(NoTypeHints)

  def commit(args: Seq[String], dirAct: File) : Unit = {
    if(args.length == 2 && args.head.equals("-m")) {
      println(makeCommit(args.tail.head, dirAct))
    } else {
      println("See usage.")
    }
  }

  // return log of the commit
  def makeCommit(message: String, dirAct: File) : String = {
    val SA = Sdir.getSA(dirAct)
    val commits = Sdir.getCommitDir(dirAct)
    val fRef = Sdir.getRef(dirAct)


    val headCommit = Sdir.getHeadCommit(dirAct);

    val headCommitHash = headCommit.getOrElse(File("")).name

    var diffsToCommit: Seq[DifferenceDir] = Seq()

    // if no diff between SA and last commit, no commit
    if (!headCommit.isEmpty) {
      diffsToCommit = Difference.diffDirectories(SA, File(headCommit.get.pathAsString + "/src"))
      if(diffsToCommit.isEmpty) {
        return "nothing to commit, working tree clean"
      }
    }

    // Create the dir for the new commit
    val sha1 = hash(SA.children.toString() + message + headCommitHash + "sgit")
    val dirNewCommit = File(commits.pathAsString + "/" + sha1).createIfNotExists(true)
    val srcNewCommit = File(dirNewCommit.pathAsString + "/src").createIfNotExists(true)
    val fNewCommit = File(dirNewCommit.pathAsString + "/commit.json")

    // Create the new commit and save it
    val SnewCommit = createNewCommit(sha1, headCommitHash, message)
    UJson.writeSerializedJson(SnewCommit, fNewCommit)

    SA.children.toSeq.map(file => file.copyToDirectory(srcNewCommit))

    // Change the pointer to the new commit
    URef.changeCurrentCommit(sha1, fRef)

    val nbDiffs = diffsToCommit.length

    // if no previous commit

    val diffsBetweenDir = countDiffBetweenDirectories(SA, File(headCommit.get.pathAsString   + "/src"))
    val nbAdd = diffsBetweenDir(0)
    val nbDelete = diffsBetweenDir(1)
    raw"""
      | [$URef.getCurrentBranchName(fRef) $sha1] $message
      | $nbDiffs file changed, $nbAdd insertions(+), $nbDelete deletions(-)
      |""".stripMargin
  }

  def countDiffBetweenDirectories(dir1 : File, dir2: File): Array[Int] = {
    var cptAdd = 0
    var cptDelete = 0
    val diffDirs = Difference.diffDirectories(dir1, dir2)

    diffDirs.map(diffDir => {
      if (diffDir.diff == DiffEnum.MODIFY) {
        var diffFiles = Difference.diffFiles(File(dir1.pathAsString + "/" + diffDir.path),
          File(dir2.pathAsString + "/" + diffDir.path))

        diffFiles.map(diffFile => {
          if (diffFile.diff == DiffEnum.ADD) {
            cptAdd += 1
          } else if(diffFile.diff == DiffEnum.DELETE) {
            cptDelete +=1
          }
        })
      }
    })

    Array(cptAdd, cptDelete)
  }

  def createNewCommit(hash: String, prevHash: String, message: String): String = {
    raw"""{
      |  "prevCommit" : $prevHash,
      |  "messsage" : $message,
      |  "author": "",
      |  "Date": "",
      |  "tag": [],
      |  "hash": $hash
      |  }
      |""".stripMargin
  }

  def hash(s: String) : String = {
    s.sha1.hex
  }
}
