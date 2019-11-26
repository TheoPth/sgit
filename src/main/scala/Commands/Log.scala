package Commands

import Commit.statDiffDir
import Commands.Diff.{computeDiffString, prepareHeader}
import Utils.Difference.DiffEnum.{ADD, DELETE, MODIFY}
import Utils.Difference.{Difference, DifferenceFile}
import Utils.MoveDir.Sdir
import Utils.archiSgit.UCommit
import better.files.File

object Log {
  def log(args: Seq[String], dirAct: File): Unit = {
    val repo = Sdir.getWD(dirAct)
    args match {
      case Seq() => print(makeLog("", repo))
      case Seq(arg) if arg == "--stat" || arg == "-p" => print(makeLog(args.head, repo))
      case _ => println("See usage")
    }
  }

  def makeLog(arg: String, repo: File): String = {
    val curCommit = Sdir.getOptHeadCommitDir(repo)

    if (curCommit.isEmpty) return ""

    val commits = Sdir.getPrecCommits(curCommit.get)

    def commitsToLog(commits: Seq[File], res: String ): String = {

      commits match {
        case Seq (last) => {
          var restmp = ""
          File.usingTemporaryDirectory() {tmp => {
            restmp = commitsToLog(Seq(), res + commitToLog(last, File(last.pathAsString + "/src"), tmp, arg))
          }}
          restmp
        }

        case first +: tail => commitsToLog(tail, res + commitToLog(first, File(first.pathAsString + "/src"), File(tail.head.pathAsString + "/src"),  arg))
        case _ => res
      }
    }
    commitsToLog(commits, "")
  }

  def commitToLog(c1: File, src1: File, src2: File, arg: String) : String = {
    val sCommit = UCommit.commitToString(c1)
    val suite: String = arg match {
      case "-p" => Diff.makeDiff(src1, src2)
      case "--stat" => Log.statCommit(src1, src2)
      case _ => ""
    }
    sCommit + "\n" + suite
  }

  def statCommit(dir1: File, dir2: File): String = {
    val diffDirectories = Difference.diffDirectories(dir2, dir1)
    val names = diffDirectories.map(diff => File(diff.path.toString).name)
    val maxSizeName = maxSizeString(names)

    diffDirectories
      .map(diffDir => {
        val v1 = File(dir1.pathAsString + "/" + diffDir.path.toString)
        val v2 = File(dir2.pathAsString + "/" + diffDir.path.toString)
        var res = ""
        diffDir.diff match {
          case DELETE => File.usingTemporaryFile() {tmp => {
            val diffs = Difference.diffFiles(v2, tmp)
            res = fileToString(v2.name, maxSizeName, diffs)
          }}
          case ADD => File.usingTemporaryFile() {tmp => {
            val diffs = Difference.diffFiles(tmp, v1)
            res = fileToString(v1.name, maxSizeName, diffs)
          }}
          case MODIFY => {
            val diffs = Difference.diffFiles(v2, v1)
            res = fileToString(v1.name, maxSizeName, diffs)
          }
        }
        res
      }).mkString +
      "\n" + statDiffDir(dir1, dir2)
  }

  def fileToString(nameFile: String, maxSizeName: Int, diffs: Seq[DifferenceFile]): String = {
    val name: String = " " + nameFile + (" " * (maxSizeName - nameFile.length))  + " | "
    val nbAdd = diffs.count(diff => diff.diff == ADD)
    val nbDel = diffs.count(diff => diff.diff == DELETE)
    name + (nbAdd + nbDel) + " " + Console.GREEN +  repeatString("+", nbAdd) + Console.RED + repeatString("-", nbDel) + Console.WHITE + "\n"
  }

  def repeatString(s: String, n: Int): String = List.fill(n)(s).mkString

  def maxSizeString(strings: Seq[String]): Int = {
    strings.maxBy(s => s.length).length
  }
}
