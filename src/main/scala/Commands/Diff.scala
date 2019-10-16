  package Commands

  import Utils.Difference.{DiffEnum, Difference, DifferenceDir, DifferenceFile}
  import Utils.Difference.DiffEnum._
  import Utils.JSON.UJson
  import Utils.MoveDir.Sdir
  import better.files.File

  case class Sdiff(string : Seq[String], a: Int, nba: Int, b: Int, nbb: Int)

  object Diff {

    def diff(repo: File): Unit = {
      println(makeDiff(repo))
    }

    def makeDiff(repo: File): String = {
      val SA = Sdir.getSA(repo)
      val WD = Sdir.getWD(repo)

      val diffDirectories = Difference.diffDirectories(WD, SA)
      val diffFiles = diffDirectories.map(diffDir => {
        val v1 = File(WD.pathAsString + "/" + diffDir.path.toString)
        val v2 = File(SA.pathAsString + "/" + diffDir.path.toString)
        Difference.diffFiles(v1, v2)
      })

      diffFiles.map( diffs => computeDiffString(diffs)).mkString("\n")
    }

    def computeDiffString(differencesFile: Seq[DifferenceFile]): String = {

      def aux(begin: Int, res : String): String = {
        val screen = findSreen(begin, differencesFile)
        screen match {
          case None => res
          case Some(screen) => {
            val s = screenToString(screen(0), screen(1), differencesFile)
            aux(screen(1), res + s)
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
      }).mkString("\n") + "\n"

        s"""$h
           |$content""".stripMargin
    }

    def computeHeading(begin: Int, end: Int, diffs: Seq[DifferenceFile]): String = {
      val debFile1 = diffs.count(diff => (diff.diff == EQUALS || diff.diff == ADD) && diff.index >= 0 && diff.index < begin) + 1
      val longFile1 = diffs.count(diff => (diff.diff == EQUALS || diff.diff == ADD) && (begin <= diff.index && diff.index <= end))

      val debFile2 = diffs.count(diff => (diff.diff == EQUALS || diff.diff == DELETE) && diff.index >= 0 && diff.index < begin) + 1
      val longFile2 = diffs.count(diff => (diff.diff == EQUALS || diff.diff == DELETE) && (begin <= diff.index && diff.index <= end))

      Console.BLUE + s"""@@ -$debFile1,$longFile1 +$debFile2,$longFile2 @@""".stripMargin + Console.WHITE
    }
  }
