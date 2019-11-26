package Commands

import Utils.Difference.{DiffEnum, Difference, DifferenceDir, DifferenceFile}
import Utils.Difference.DiffEnum._
import Utils.JSON.UJson
import Utils.MoveDir.Sdir
import better.files.File

case class Sdiff(string : Seq[String], a: Int, nba: Int, b: Int, nbb: Int)

object Diff {

  def diff(repo: File): Unit = {
    val SA = Sdir.getSA(repo)
    val WD = Sdir.getWD(repo)
    print(makeDiff(WD, SA))
  }


  def makeDiff(dir1: File, dir2: File): String = {

    val diffDirectories = Difference.diffDirectories(dir2, dir1)

    diffDirectories
      .map(diffDir => {
        val v1 = File(dir1.pathAsString + "/" + diffDir.path.toString)
        val v2 = File(dir2.pathAsString + "/" + diffDir.path.toString)
        var res = ""
        diffDir.diff match {
          case DELETE => File.usingTemporaryFile() {tmp => {
            val diffs = Difference.diffFiles(tmp, v2)
             res = prepareHeader(v2.name, "deleted file\n") + computeDiffString(diffs)
          }}
          case ADD => File.usingTemporaryFile() {tmp => {
            val diffs = Difference.diffFiles(v1, tmp)
            res = prepareHeader(v1.name, "add file\n") + computeDiffString(diffs)
          }}
          case MODIFY => {
            val diffs = Difference.diffFiles(v1, v2)
            res = prepareHeader(v1.name, "") + computeDiffString(diffs)
          }
        }
        res
      }).mkString
  }

  def prepareHeader(fileName: String, addInf: String): String = {
    "diff --git a/" + fileName + " " + "b/" + fileName + s"\n" +
      addInf +
      "--- a/" + fileName + s"\n" +
      "+++ b/" + fileName + s"\n"
  }

  def computeDiffString(differencesFile: Seq[DifferenceFile]): String = {

    def aux(begin: Int, res : String): String = {
      val screen = findSreen(begin, differencesFile)
      screen match {
        case None => res
        case Some(screen) => {
          val s = screenToString(screen(0), screen(1), differencesFile)
          aux(screen(1) + 1, res + s)
        }
      }
    }

    aux(0, "")
  }


  /*
  cut diff and extract one Seq of differenceFile
  return the cut and the reste of the differenceFile
   */
  def findSreen(begin: Int, differenceFiles: Seq[DifferenceFile]) : Option[Seq[Int]] = {

    def aux(first: Int, cur: Int, lastModif: Int) : Option[Seq[Int]] = {
      val length = cur - first

      // Stop conditions
      if (cur >= differenceFiles.length && lastModif == -1) {
        return None
      }

      if ((cur >= differenceFiles.length && lastModif != -1) || lastModif >= 2) {
        return Some(Seq(first, cur))
      }

      differenceFiles(cur).diff match {
        case EQUALS => {
          var lModif = lastModif
          if (lastModif > -1) lModif+=1

          if (length >= 3 && lastModif == -1) {
            aux(first + 1, cur + 1, lModif)
          } else {
            aux(first, cur + 1, lModif)
          }
        }
        case _ => aux(first, cur + 1, 0)
      }
    }

    aux(begin, begin, -1)
  }

  def screenToString(begin: Int, end: Int, diffs: Seq[DifferenceFile]): String = {
    val h = computeHeading(begin, end, diffs)
    val content = diffs.filter(diff => begin <= diff.index && diff.index <= end).map(diff => {
      val deb  = diff.diff match {
        case EQUALS => " "
        case DELETE => Console.GREEN + "+"
        case ADD => Console.RED + "-"
      }
      deb + diff.content + Console.WHITE
    }).mkString("\n")

    s"""$h
       |$content
       |""".stripMargin
  }


  def computeHeading(begin: Int, end: Int, diffs: Seq[DifferenceFile]): String = {
    val debFile1 = diffs.count(diff => (diff.diff == EQUALS || diff.diff == ADD) && diff.index >= 0 && diff.index < begin) + 1
    val longFile1 = diffs.count(diff => (diff.diff == EQUALS || diff.diff == ADD) && (begin <= diff.index && diff.index <= end))

    val debFile2 = diffs.count(diff => (diff.diff == EQUALS || diff.diff == DELETE) && diff.index >= 0 && diff.index < begin) + 1
    val longFile2 = diffs.count(diff => (diff.diff == EQUALS || diff.diff == DELETE) && (begin <= diff.index && diff.index <= end))

    Console.BLUE + s"""@@ -$debFile1,$longFile1 +$debFile2,$longFile2 @@""".stripMargin + Console.WHITE
  }
}
