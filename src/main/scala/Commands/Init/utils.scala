package Commands.Init

import better.files.File

object utils {
  def copyAllChildrenToExcept (child: Iterator[File], dir: File, except: Seq[File]): Unit = {
    if (child.hasNext) {
      var f = child.next();
      if (!except.contains(f)) {
        f.copyToDirectory(dir)
      }
      copyAllChildrenToExcept(child, dir, except);
    }
  }
}
