package Commands.Commit

import Utils.Difference.{DiffEnum, Difference, DifferenceDir}
import Utils.JSON.{UJson, URef}
import Utils.MoveDir.Sdir
import better.files.File
import com.roundeights.hasher.Implicits._
import org.json4s.NoTypeHints
import org.json4s.native.Serialization

import scala.io.Source
import scala.language.postfixOps


object Commit {
  implicit val formats = Serialization.formats(NoTypeHints)

  def commit(args: Seq[String], dirAct: File) : Unit = {
    if(args.head.equals("-m")) {
      println(makeCommit(args.tail.mkString(" "), dirAct))
    } else {
      println("See usage.")
    }
  }

  // return log of the commit
  def makeCommit(message: String, dirAct: File) : String = {
    val SA = Sdir.getSA(dirAct)
    val commits = Sdir.getCommitDir(dirAct)
    val fRef = Sdir.getRef(dirAct)


    val headCommit = Sdir.getOptHeadCommitDir(dirAct);

    val headCommitHash = headCommit.getOrElse(File("")).name

    val diffsToCommit: Seq[DifferenceDir] = headCommit match {
      case Some(commit) =>  {
        Difference.diffDirectories(SA, File(commit + "/src"))
      }
      case None => {
        var diffs: Seq[DifferenceDir] = Seq()
        File.usingTemporaryDirectory() {
          tmpDir => {
            diffs = Difference.diffDirectories(SA, tmpDir)
          }
        }
        diffs
      }
    }

    if (diffsToCommit.isEmpty) return "nothing to commit, working tree clean"

    // Create the dir for the new commit
    val sha1 = hash(SA.children.toString() + message + headCommitHash + "sgit")
    val dirNewCommit = File(commits.pathAsString + "/" + sha1).createIfNotExists(true)
    val srcNewCommit = File(dirNewCommit.pathAsString + "/src").createIfNotExists(true)
    val fNewCommit = File(dirNewCommit.pathAsString + "/commit.json")

    // Create the new commit and save it
    val SnewCommit = createNewCommit(sha1, headCommitHash, message)
    UJson.writeSerializedJson(SnewCommit, fNewCommit)

    // Copy SA in the commit
    SA.children.toSeq.map(file => file.copyToDirectory(srcNewCommit))

    // Change the pointer to the new commit
    URef.changeCurrentCommit(sha1, fRef)

    val nbDiffs = diffsToCommit.length

    // Prepare log
    // if no previous commit
    val diffsBetweenDir: Array[Int] = headCommit match {
      case Some(f) =>  {
        val srcHeadCommit = File(f.pathAsString + "/src")
        countDiffBetweenDirectories(SA, srcHeadCommit)
      }
      case None => {
        var diffs:Array[Int] = Array()
        File.usingTemporaryDirectory() {
          tmpDir => {
            diffs = countDiffBetweenDirectories(SA, tmpDir)
          }
        }
        diffs
      }
    }

    val nbAdd = diffsBetweenDir(0)
    val nbDelete = diffsBetweenDir(1)
    val currentBranchName = URef.getCurrentBranchName(fRef)
    val shaCommit = sha1.slice(0, 6)

    raw"""[$currentBranchName $shaCommit] $message
      |$nbDiffs file changed, $nbAdd insertions(+), $nbDelete deletions(-)""".stripMargin
  }


  def countDiffBetweenDirectories(dir1 : File, dir2: File): Array[Int] = {
    var cptAdd = 0
    var cptDelete = 0
    val diffDirs = Difference.diffDirectories(dir2, dir1)

    diffDirs.map(diffDir => {
      val f1: File = File(dir1.pathAsString + "/" + diffDir.path)
      val f2: File = File(dir2.pathAsString + "/" + diffDir.path)
      if (diffDir.diff == DiffEnum.MODIFY) {
        val diffFiles = Difference.diffFiles(File(dir1.pathAsString + "/" + diffDir.path),
          File(dir2.pathAsString + "/" + diffDir.path))

        diffFiles.map(diffFile => {
          if (diffFile.diff == DiffEnum.ADD) {
            cptAdd += 1
          } else if(diffFile.diff == DiffEnum.DELETE) {
            cptDelete +=1
          }
        })
      } else if (diffDir.diff == DiffEnum.DELETE) {
        cptDelete += nbLinesFile(f2)
      } else if (diffDir.diff == DiffEnum.ADD) {
        cptAdd += nbLinesFile(f1)
      }
    })

    Array(cptAdd, cptDelete)
  }

  def nbLinesFile(file: File): Int = {
    Source.fromFile(file.pathAsString).getLines.length
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
