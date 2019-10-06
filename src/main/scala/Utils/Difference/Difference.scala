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
    Compute diff between two directories (only file deleted and modified)
  */

  def diffDirectories(dir1 : File, dir2 : File): Seq[DifferenceDir] = {
    val res1 = diffDirectoriesAddModify(dir1, dir2)
    val res2 = diffDirectoriesAddModify(dir2, dir1)
    return concatDirDiff(res1, res2)
  }

  def diffDirectoriesAddModify (dir1 : File, dir2 : File): Seq[DifferenceDir] = {
    val pathDir1 = dir1.path
    val pathDir2 = dir2.path

    def aux (d1: Seq[File]): Seq[DifferenceDir] = {
      if (d1.isEmpty) {
        return Seq()
      }

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
    return aux(dir1.children.toSeq)
  }


  def concatDirDiff(diffs1: Seq[DifferenceDir], diffs2: Seq[DifferenceDir]) : Seq[DifferenceDir] = {
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
}