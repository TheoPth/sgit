package Commands

import Utils.Difference.{DiffEnum, DifferenceDir, DifferenceFile}
import Utils.Difference.DiffEnum._
import better.files.File

class Diff {

  def add(repo: File): Unit = {
    println(makeAdd(repo))
  }

  def makeAdd(repo: File): String = {

  }

  def computeFirstDiff(file1: Seq[String], file2: Seq[String], diffDirs: Seq[DifferenceDir], beginIndex: Int): Seq[String] = {

    def aux(diffs: Seq[DifferenceFile], curI: Int, nbLoopWithoutDiffs: Int, captureDiffs: Seq[String]): Seq[String] = {
      // Indexs
      val i1 = nbOfDiffBetween(ADD, beginIndex, curI, diffs) + curI
      val i2 = nbOfDiffBetween(DELETE, beginIndex, curI, diffs) + curI
      val isCapturing = nbLoopWithoutDiffs !=  captureDiffs.length

      // Stop conditions
      if (nbLoopWithoutDiffs > 3 && isCapturing) {
        
      }

      if (file1.length <= i1 && file2.length <= i2) {

      }

      // Treatements
      if (diffs.head.index == curI) {
        diffs.head.diff match {
          case DELETE => aux(diffs.tail, curI + 1, 0, captureDiffs :+ Console.GREEN + "+" + diffs.head.content)
          case ADD => aux(diffs.tail, curI + 1, 0, captureDiffs :+ Console.RED + "-" + diffs.head.content)
        }
      } else {
        if (!isCapturing) {
          aux(diffs, curI + 1, nbLoopWithoutDiffs + 1, captureDiffs.tail :+ file1(i1))
        } else {
          aux(diffs, curI + 1, nbLoopWithoutDiffs + 1, captureDiffs :+ file1(i1))
        }
      }
    }

    aux(diffDirs, beginIndex)
  }

  // Returns the number of additions between indexes
  def nbOfDiffBetween(diffType: DiffEnum.Value, beginIndex: Int, endIndex: Int, diffs: Seq[DifferenceFile]): Unit = {
    diffs.count(div => div.diff == diffType && beginIndex <= div.index && div.index < endIndex)
  }
}
