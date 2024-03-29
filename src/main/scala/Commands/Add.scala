package Commands

import java.nio.file.{Path, Paths}

import Utils.Difference.{Difference, DifferenceDir}
import Utils.MoveDir.{MoveDir, Sdir}
import better.files.File
import org.json4s._
import org.json4s.native.Serialization

object Add {
  /*
    Add fonc save diff between WD and last commit
   */

  implicit val formats = Serialization.formats(NoTypeHints)

  def add(args: Seq[String], dirAct: File) : Unit = {
    if (args.length <= 0) {
      println(raw"""Nothing specified, nothing added.
                   |Maybe you wanted to say 'sgit add .'?""".stripMargin)

    } else {
      makeAdd(args, dirAct)
    }
  }

  def makeAdd(args: Seq[String], dirAct: File) : Unit = {
    val WD = Sdir.getWD(dirAct)
    val SA = Sdir.getSA(dirAct)

    // retrieve all files ( from the path where the commands was triggered )
    val relativesPath1 = getRelativePathFromRegexOrNames(args, dirAct)

    // don't forget files deleted in the WD
    val relativesPath2 = getRelativePathFromRegexOrNames(args, SA)

    var relativesPath = unionPath(relativesPath1, relativesPath2)

    // if path is not at the root
    if(WD.path != dirAct.path) {
      // path to add to get relatives paths from WD
      val pathToWD: Path = MoveDir.toRelativePath(WD.path, dirAct.path)

      // Paths are relatives of root of folder
      relativesPath = MoveDir.changeBaseDirForSeqRelativePath(relativesPath, pathToWD)
    }

    MoveDir.copyFilesRelativePathToDir(relativesPath, WD, SA)
  }

  def unionPath(seq1: Seq[Path], seq2: Seq[Path]): Seq[Path] = {
    val s = seq1.filter(path => !seq2.contains(path))
    s ++ seq2
  }


  /*
  Compute diff between two dirs. Test only files or dir in the Seq.
   */
  def computeAllDiffAndConcat(pathToTest: Seq[Path], dir1 : File, dir2 : File) : Seq[DifferenceDir] = {

    def aux(paths: Seq[Path], acc: Seq[DifferenceDir]): Seq[DifferenceDir] = {
      if (paths.isEmpty) {
        acc
      } else if (paths.head == ".sgit") {
        aux(paths.tail, acc)
      } else {

        val fileToCompute = File(dir1.pathAsString + "/" + paths.head)
        var diffs : Seq[DifferenceDir] = Seq()

        if (fileToCompute.isDirectory) {
          diffs = Difference.diffDirectories(fileToCompute, dir2)
        } else {

          val diff = Difference.computeDiffBetweenTwoFiles(dir1, dir2, paths.head)


          diff match {
            case Some(diff) => diffs = Seq(DifferenceDir(paths.head.toString, diff))
            case None =>
          }

        }

        aux(paths.tail, Difference.unionDirDiff(diffs, acc))
      }
    }

    aux(pathToTest, Seq())
  }

  // Args can be : <filename/filenames or . or regexp>
  // Dir : Base directory
  def getRelativePathFromRegexOrNames(args: Seq[String], baseDir: File) : Seq[Path] = {
    // Regex or file name
    def aux (args: Seq[String], acc: Seq[Path]) : Seq[Path] = {
      if (args.isEmpty) {
        acc
      } else {
        var files: Seq[File] = Seq()

        if (args.head.equals(".")) {
          files = baseDir.children.toSeq
        } else {
          files = baseDir.glob(args.head, includePath = true).toSeq
        }

        files = files.filter(file => file.name != ".sgit")

        val relativesPaths = MoveDir.seqFilesToSeqRelativePath(files, baseDir)
        aux(args.tail, acc ++ relativesPaths)
      }
    }

    aux(args, Seq())
  }
}
