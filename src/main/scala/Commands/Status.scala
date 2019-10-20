package Commands

import Utils.Difference.{DiffEnum, Difference, DifferenceDir}
import Utils.JSON.{ORef, UJson, URef}
import better.files.File
import DiffEnum._
import Utils.MoveDir.Sdir

object Status {

  def status(dirAct: File) : Unit = {
    val WD = Sdir.getWD(dirAct)
    println(makeStatus(WD))
  }

  def makeStatus(repo: File) : String = {
    val oRef = UJson.readDeserializedJson[ORef](Sdir.getRef(repo))
    val branchName = URef.getCurrentBranchName(oRef)
    val commit = Sdir.getOptHeadCommit(repo)

    // Retrieve diffs not COMMITED
    var notCommited: String = uncommited(repo)

    if (!notCommited.equals("")) {
      notCommited =
        raw"""Changes to be committed:
             |
             |$notCommited
             |""".stripMargin  + Console.WHITE
    }

    var noCommit: String = ""
    if (commit.isEmpty) {
      noCommit = "No commits yet\n"
    }

    // Retrieve diffs no STAGED (in red)
    var notStaged: String = unstagged(repo)

    if (!notStaged.equals("")) {
      notStaged =
        raw"""Changes not staged for commit:
          | (use "sgit add/rm <file>..." to update what will be committed)
          |
          |$notStaged
          |""".stripMargin + Console.WHITE
    }

    // Retrieve diff for UNTRACKED file
    var notTracked: String = untracked(repo)

    if (!notTracked.equals("")) {
      notTracked =
        raw"""Untracked files:
          |  (use "sgit add <file>..." to include in what will be committed)
          |
          |$notTracked
          |""".stripMargin + Console.WHITE
    }

    var infCommit: String = ""
    if (notCommited.equals("")) {
      infCommit +=
        raw"""nothing to commit (create/copy files and use "sgit add" to track)""".stripMargin + Console.WHITE
    }

     raw"""On branch $branchName
      |$noCommit$notCommited$notStaged$notTracked$infCommit""".stripMargin
  }

  def uncommited (repo: File): String = {
    val SA = Sdir.getSA(repo)
    val optCommit = Sdir.getOptHeadCommit(repo)

    def aux(SA: File, commit: File): String = {
      val diffsNotCommited = Difference.diffDirectories(SA, commit)
      var notCommited: String = ""

      diffsNotCommited.foreach(diff => {
        val fName = diff.path
        diff.diff match {
          case ADD => notCommited += "\t" + Console.GREEN + "deleted: " + fName + "\n"
          case MODIFY => notCommited += "\t" + Console.GREEN + "modified: " + fName + "\n"
          case DELETE => notCommited += "\t" + Console.GREEN + "new file: " + fName + "\n"
        }
      })
      notCommited
    }


    // if no commit, use a empty dir to compute the diff
      optCommit match {
      case Some(commit) => {
        aux(SA, commit)
      }
      case None => {
        var diff: String = ""
        File.usingTemporaryDirectory() {
          tmpDir => {
            diff = aux(SA, tmpDir)
          }
        }
        return diff
      }
    }


  }

  def unstagged(repo: File): String = {
    val SA = Sdir.getSA(repo)
    val WD = Sdir.getWD(repo)

    val diffsNotStaged = Difference.diffDirectories(WD, SA)

    var notStaged: String = ""
    diffsNotStaged.foreach(diff => {
      val fName = File(diff.path).name

      diff.diff match {
        case MODIFY => notStaged += "\t" + Console.RED + "modified: " + fName + "\n"
        case ADD => notStaged += "\t" + Console.RED + "deleted: " +  fName + "\n"
        case _ => ""
      }
    })

    notStaged
  }

  def untracked(repo: File): String = {
    val SA = Sdir.getSA(repo)
    val WD = Sdir.getWD(repo)
    val diffsNotTracked = Difference.getDiffOnTop(WD, SA);
    var notTracked: String = ""

    diffsNotTracked.foreach(diff => {
      val fName = diff.path
       diff.diff match {
        case ADD => notTracked += "\t" + Console.RED + fName + "\n"
        case _ => ""
      }
    })

    notTracked
  }
}
