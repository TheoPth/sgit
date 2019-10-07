package Commands

import java.nio.file.Path

import Utils.MoveDir.MoveDir
import better.files.File

object Add {
  /*
    Add fonc save diff between WD and last commit
   */

  def add(args: Seq[String], dirAct: File) : Unit = {
    val pathToSgit = MoveDir.findRelativePathSgit(dirAct.path);

    val relativesPath = getRelativePathFromRegexOrNames(args, pathToSgit)



    // All files selected in the args
    //val files =
  }

  // Args can be : <filename/filenames or . or regexp>
  // Dir : Base directory
  def getRelativePathFromRegexOrNames(args: Seq[String], baseDir: File) : Seq[Path] = {
    // Regex or file name
    def aux (args: Seq[String], acc: Seq[Path]) : Seq[Path] = {
      if (args.isEmpty) {
        acc
      } else {
        val files = baseDir.glob(args.head, includePath = true).toSeq
        val relativesPaths = MoveDir.seqFilesToSeqRelativePath(files, baseDir)
        aux(args.tail, acc ++ relativesPaths)
      }
    }

    aux(args, Seq())
  }
}
