package Utils.Difference

import java.nio.file.{Path, Paths}

import Utils.Difference.Difference.diffDirectoriesAddModify
import better.files.File
import org.json4s.native.Json

import scala.io.Source

case class DifferenceFile(diff: DiffEnum.Value, index: Int, content: String) {}
case class DifferenceDir(path: String, diff: DiffEnum.Value){}

object Difference {
  /*
  Compute the delta between two files. Delta is modifications to apply to text 1 to get text 2.
  One difference between files is stored as Difference (class)
   */
  def diffFiles (f1: File, f2: File) : Seq[DifferenceFile] = {
    val text1 = Source.fromFile(f1.pathAsString).getLines.toSeq
    val text2 = Source.fromFile(f2.pathAsString).getLines.toSeq

    diffSeqString(text1, text2)
  }

  def optDiffFile(file1: Option[File], file2 : Option[File]): Seq[DifferenceFile] = {
    (file1, file2) match {
      case (Some(f1), Some(f2)) => {
        diffFiles(f1, f2)
      }
      case (Some(f1), None) => {
        var diffs: Seq[DifferenceFile] = Seq()
        File.usingTemporaryFile() {
          tmpFile => {
            diffs = diffFiles(f1, tmpFile)
          }
        }
        diffs

      }

      case (None, Some(f2)) => {
        var diffs: Seq[DifferenceFile] = Seq()
        File.usingTemporaryFile() {
          tmpFile => {
            diffs = diffFiles(tmpFile, f2)
          }
        }
        diffs
      }

      case (None, None) => Seq()
    }
  }

  def diffSeqString(text1: Seq[String], text2: Seq[String]) : Seq[DifferenceFile] = {
    def aux(t1: Seq[String], t2: Seq[String], diffs: Seq[DifferenceFile]) : Seq[DifferenceFile] = {
      // Is both empty, all differences have been computed
      if (t1.isEmpty && t2.isEmpty) {
        return diffs
      }

      if (t1.isEmpty) {
        return aux(t1, t2.tail, diffs :+ DifferenceFile(DiffEnum.ADD, diffs.length, t2.head))
      }

      if (t2.isEmpty) {
        return aux(t1.tail, t2, diffs :+ DifferenceFile(DiffEnum.DELETE, diffs.length, t1.head))
      }


      if (t1.head.equals(t2.head)) {
        aux(t1.tail, t2.tail, diffs :+ DifferenceFile(DiffEnum.EQUALS, diffs.length, t2.head))
      } else {
        // If not equals, we can get delete text 1 line or add text 2 line. We cannot know what the vest choice is
        // So try both and keep the better one (solution with less diff)
        val pos1 = aux(t1, t2.tail, diffs :+ DifferenceFile(DiffEnum.ADD, diffs.length, t2.head))
        val pos2 = aux(t1.tail, t2, diffs :+ DifferenceFile(DiffEnum.DELETE, diffs.length, t1.head))

        if (pos1.length < pos2.length) pos1 else pos2
      }
    }

    aux(text1, text2, Seq());
  }


  /*
    Compute which files are added or modify between dir1 and dir2 recursively
    ignore .sgit directori
     */
  def diffDirectories(dir1 : File, dir2 : File): Seq[DifferenceDir] = {
    val res1 = diffDirectoriesAddModify(dir1, dir2)
    val res2 = diffDirectoriesAddModify(dir2, dir1)
    computeDirDiff(res1, res2)
  }

  /*
    Compute which files are added or modify between dir1 and dir2 recursively
     */
  def optDiffDirectories(dir1 : Option[File], dir2 : Option[File]): Seq[DifferenceDir] = {
    (dir1, dir2) match {
      case (Some(f1), Some(f2)) => {
        diffDirectories(f1, f2)
      }
      case (Some(f1), None) => {
        var diffs: Seq[DifferenceDir] = Seq()
        File.usingTemporaryDirectory() {
          tmpDir => {
            diffs = diffDirectoriesAddModify(f1, tmpDir)
          }
        }
        diffs
      }
      case (None, Some(f2)) => {
        var diffs: Seq[DifferenceDir] = Seq()
        File.usingTemporaryDirectory() {
          tmpDir => {
            val addMod = diffDirectoriesAddModify(tmpDir, f2)
            diffs = computeDirDiff(Seq(), addMod)
          }
        }
        diffs
      }
      case (None, None) => Seq()
    }
  }

  /*
    Compute diffs between two directories (only file deleted and modified)
  */
  def diffDirectoriesAddModify (dir1 : File, dir2: File): Seq[DifferenceDir] = {
    val pathDir1 = dir1.path
    val pathDir2 = dir2.path

    /*
    iterate over d1
     */
    def aux (d1: Seq[File]): Seq[DifferenceDir] = {
      if (d1.isEmpty) {
        return Seq()
      }

      if (d1.head.name.equals(".sgit")) {
        return aux(d1.tail)
      }

      // Stored as relative path
      val pathRelatif = pathDir1.relativize(d1.head.path)
      val pathToFileInDir2 = pathDir2 + "/" + pathRelatif.toString

      // File to compare
      val file1 = d1.head

      if (file1.isDirectory) {
        return aux(d1.head.children.toSeq) ++ aux(d1.tail)
      }

      val diff = computeDiffBetweenTwoFiles(dir1, dir2, pathRelatif)

      diff match {
        case Some(diff) =>  aux(d1.tail) :+ DifferenceDir( pathRelatif.toString, diff)
        case None => aux(d1.tail)
      }
    }

    aux(dir1.children.toSeq)
  }

  // Return a diff dir if files have'nt the same status
  // path is the path to store in the differenceDir (path of File is always absolute)
  // Null if no diff between files
  def computeDiffBetweenTwoFiles(dir1: File, dir2: File, pathRelative: Path): Option[DiffEnum.Value]= {
    val file1 = File(dir1.pathAsString + "/" + pathRelative)
    val file2 = File(dir2.pathAsString + "/" + pathRelative)

    if (!file2.exists) {
      return Some(DiffEnum.DELETE)
    }

    if(!file1.exists) {
      return Some(DiffEnum.ADD)
    }

    if (file1.isSameContentAs(file2)) {
      None
    } else {
      Some(DiffEnum.MODIFY)
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

  // Concat two Seqs of dirDiff and delete redondants dirDiffs
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

  // Compute the difference between to Seq of difference (diffs1 = WD and diffs2 = SA)
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

  // Compute diffs but not recursively ( compute only add file or dir in the current dir)
  def getDiffOnTop(dir1: File, dir2: File) : Seq[DifferenceDir] = {
    val pathDir1 = dir1.path
    val pathDir2 = dir2.path

    var added: Seq[DifferenceDir] = Seq()

    dir1.children.toSeq.foreach( child => {

      // Stored as relative path
      var pathRelatif = pathDir1.relativize(child.path)
      val pathToFileInDir2 = pathDir2 + "/" + pathRelatif.toString

      if (!File(pathToFileInDir2).exists && child.name != ".sgit") {
        if (child.isDirectory) {
          added = added :+ DifferenceDir(pathRelatif.toString +"/", DiffEnum.ADD)
        } else {
          added = added :+ DifferenceDir(pathRelatif.toString, DiffEnum.ADD)
        }
      }
    })

    added
  }
}


