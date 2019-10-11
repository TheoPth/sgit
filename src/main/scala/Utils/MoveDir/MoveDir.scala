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


  // Change relative path to new File ref
  // newBaseDir is the path to go to new dir in relative
  def changeBaseDirForSeqRelativePath(paths: Seq[Path], pathToNewDir: Path): Seq[Path] = {

    def aux(oldPaths: Seq[Path], newPaths: Seq[Path]) : Seq[Path] = {
      if(oldPaths.isEmpty) {
        newPaths
      } else {
        val newPath = Paths.get(pathToNewDir.toString + "/" + oldPaths.head.toString)
        aux(oldPaths.tail, newPaths :+ newPath)
      }
    }

    aux(paths, Seq())
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
        aux(files.tail)
      }
    }

    if (!dir.isEmpty) {
      aux(dir.children.toSeq);
    } else {
      false
    }
  }

  // Copy files from dir1 to dir2 with relatives path, delete the file in the dir2 if not in the dir1
  def copyFilesRelativePathToDir(relativePaths: Seq[Path], dir1: File, dir2: File): Unit = {
    relativePaths.map(path => {

      val f1 = File(dir1.pathAsString + "/" + path)
      val f2 = File(dir2.pathAsString + "/" + path)

      // if the file has been added
      if (f1.exists && !f2.exists) {

        var target: File = null;
        if (path.getParent != null) {
          target = File(dir2.pathAsString + "/" + path.getParent).createIfNotExists(true, true)
        } else {
          target = dir2
        }

        f1.copyToDirectory(target)

        // if the file has been modified
      } else if (f1.exists && f2.exists && !f1.isSameContentAs(f2)) {
        f2.delete()
        f1.copyToDirectory(dir2)

        // if the file has been deleted
      } else {
        f2.delete();
      }
    })
  }
}
