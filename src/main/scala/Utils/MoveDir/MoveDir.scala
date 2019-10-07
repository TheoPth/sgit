package Utils.MoveDir

import java.nio.file.{Path, Paths}

import better.files.File

object MoveDir {


  /*
    Find the relative path of .sgit dir in the actual dir or above
   */
  def findPathSgit(basePath : Path) : Path = {
    val baseDir = File(basePath);

    def aux (dir: File) : Path = {
      if (dir != null) {
        if (containsFileWithName(dir, ".sgit")) {
          Paths.get(dir.pathAsString + "/.sgit")
        }
        else {
          aux(dir.parent)
        }
      } else {
        null
      }
    }

    return aux(baseDir)
  }

  def findRelativePathSgit(basePath : Path) : Path = {
    val pathFounded  =  findPathSgit(basePath);
    if (pathFounded == null) {
      null
    } else{
      toRelativePath(basePath,pathFounded);
    }
  }

  def toRelativePath(path1: Path, path2: Path): Path = {
    path1.relativize(path2);
  }

  def seqFilesToSeqRelativePath(files: Seq[File], dir: File): Seq[Path] = {
    val seqAbsolutePath = seqFilesToSeqAbsolutePath(files)
    seqAbsolutePathToRelativePath(seqAbsolutePath, dir);
  }

  def seqFilesToSeqAbsolutePath(files: Seq[File]) : Seq[Path] = {
    def aux (files: Seq[File], acc: Seq[Path]): Seq[Path] = {
      if (files.isEmpty) {
        acc
      } else {
        aux (files.tail, acc :+ files.head.path)
      }
    }

    aux(files, Seq())
  }

  def seqAbsolutePathToRelativePath(paths: Seq[Path], dir: File): Seq[Path] = {
    def aux(paths: Seq[Path], acc: Seq[Path]) : Seq[Path] = {
      if (paths.isEmpty) {
        acc
      } else {
        aux(paths.tail, acc :+ toRelativePath(dir.path, paths.head))
      }
    }

    aux(paths, Seq())
  }

  def containsFileWithName (dir: File, name: String) : Boolean = {
    def aux (files: Seq[File]): Boolean = {
      if (files.isEmpty) {
        return false
      }
      if (files.head.name.equals(name)) {
        true
      } else {
        false || aux(files.tail)
      }
    }

    if (!dir.isEmpty) {
      aux(dir.children.toSeq);
    } else {
      false
    }

  }

}
