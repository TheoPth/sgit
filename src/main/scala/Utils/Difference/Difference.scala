package Utils.Difference

import java.nio.file.Path

import better.files.File

case class DifferenceFile(diff: DiffEnum.Value, index: Int, content: String) {}
case class DifferenceDir(path: String, diff: DiffEnum.Value){}

object Difference {
  /*
  Compute the delta between two files. Delta is modifications to apply to text 1 to get text 2.
  One difference between files is stored as Difference (class)
   */
  def diffFiles (text1: Seq[String], text2: Seq[String]) : Seq[DifferenceFile] = {
    def aux(t1: Seq[String], t2: Seq[String], diffs: Seq[DifferenceFile], index: Int) : Seq[DifferenceFile] = {
      // Is both empty, all differences have been computed
      if (t1.isEmpty && t2.isEmpty) {
        return diffs
      }

      if (t1.isEmpty) {
        return aux(t1, t2.tail, diffs :+ DifferenceFile(DiffEnum.ADD, index, t2.head), index + 1)
      }

      if (t2.isEmpty) {
        return aux(t1.tail, t2, diffs :+ DifferenceFile(DiffEnum.DELETE, index, t1.head), index + 1)
      }


      if (t1.head.equals(t2.head)) {
        return aux(t1.tail, t2.tail, diffs, index + 1)
      } else {
        // Id not equals, we can get delete text 1 line or add text 2 line. We cannot know what the vest choice is
        // So try both and keep the better one (solution with less diff)
        var pos1 = aux(t1, t2.tail, diffs :+ DifferenceFile(DiffEnum.ADD, index, t2.head), index + 1)
        var pos2 = aux(t1.tail, t2, diffs :+ DifferenceFile(DiffEnum.DELETE, index, t1.head), index + 1)
        if (pos1.length < pos2.length) pos1 else pos2
      }
    }

    return aux(text1, text2, Seq(), 0);
  }


  /*
    Compute wich files are added or modify between dir1 and dir2 recursively
     */
  def diffDirectories(dir1 : File, dir2 : File): Seq[DifferenceDir] = {
    val res1 = diffDirectoriesAddModify(dir1, dir2)
    val res2 = diffDirectoriesAddModify(dir2, dir1)
    computeDirDiff(res1, res2)
  }

  /*
    Compute diffs between two directories (only file deleted and modified)
  */
  def diffDirectoriesAddModify (dir1 : File, dir2 : File): Seq[DifferenceDir] = {
    val pathDir1 = dir1.path
    val pathDir2 = dir2.path

    /*
    iterate over d1
     */
    def aux (d1: Seq[File]): Seq[DifferenceDir] = {
      if (d1.isEmpty) {
        return Seq()
      }

      // Stored as relative path
      val pathRelatif = pathDir1.relativize(d1.head.path)
      val pathToFileInDir2 = pathDir2 + "/" + pathRelatif.toString

      // File to compare
      val file1 = d1.head
      val file2  = File(pathToFileInDir2)

      if (file1.isDirectory) {
        return aux(d1.head.children.toSeq) ++ aux(d1.tail)
      }

      if (!file2.exists)
        return aux(d1.tail) :+ DifferenceDir(pathRelatif.toString, DiffEnum.DELETE)

      if (file1.isSameContentAs(file2)) {
        return aux (d1.tail)
      } else {
        return aux (d1.tail) :+ DifferenceDir(pathRelatif.toString, DiffEnum.MODIFY)
      }
    }

    if (dir1.isDirectory) {
      aux(dir1.children.toSeq)
    } else {
      aux (Seq(dir1))
    }

  }

  // Add the delete file from diffs1 to diffs2 (Added file in diffs2 are deleted files in diffs1)
  def computeDirDiff(diffs1: Seq[DifferenceDir], diffs2: Seq[DifferenceDir]) : Seq[DifferenceDir] = {
    def aux (v2: Seq[DifferenceDir]) : Seq [DifferenceDir] = {
      if (!v2.isEmpty) {
        if (v2.head.diff == DiffEnum.DELETE) {
          return DifferenceDir(v2.head.path, DiffEnum.ADD) +: aux(v2.tail)
        } else {
          return aux(v2.tail)
        }
      } else {
        return Seq()
      }
    }
    return diffs1 ++ aux(diffs2)
  }

  // Concat two Seqs of dirDiff and delete redondants7 dirDiffs
  def unionDirDiff(diffs1: Seq[DifferenceDir], diffs2: Seq[DifferenceDir]) : Seq[DifferenceDir] = {

    def aux(dd1: Seq[DifferenceDir], acc : Seq[DifferenceDir]): Seq[DifferenceDir] = {
      if (dd1.isEmpty) {
        acc ++ diffs2
      } else {
        if (!diffs2.contains(dd1.head)) aux(dd1.tail, acc :+ dd1.head) else aux(dd1.tail, acc)
      }
    }

    aux(diffs1, Seq())
  }

  // Compute the difference between to Seq of difference (diffs1 = WD and diffs2 = SA
  def diffOfDiffDir(diffs1: Seq[DifferenceDir], diffs2: Seq[DifferenceDir]) : Seq[DifferenceDir] = {
    def aux(d1: Seq[DifferenceDir]) : Seq [DifferenceDir] = {
        if (d1.isEmpty){
          return Seq()
        }
        if (!diffs2.contains(d1.head)) {
           aux(d1.tail) :+ d1.head
        } else {
           aux(d1.tail)
        }
    }
     aux(diffs1)
  }
}


