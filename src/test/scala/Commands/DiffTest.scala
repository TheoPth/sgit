package Commands

import Utils.Difference.Difference
import better.files.File
import org.scalatest.FunSuite

class DiffTest extends FunSuite {
  test("findSreen - no screen 1") {
    val dirTestPath = System.getProperty("user.dir") + "/../findScreen";
    val dirTest = File (dirTestPath).createIfNotExists(true)


    val f1 = File(dirTestPath + "/f1").createIfNotExists().overwrite("")
    val f2 = File(dirTestPath + "/f2").createIfNotExists().overwrite("")

    val diffs = Difference.diffFiles(f1, f2)

    dirTest.delete()
    assert (Diff.findSreen(0, diffs) == None)
  }

  test("findSreen - one screen") {
    val dirTestPath = System.getProperty("user.dir") + "/../findScreen";
    val dirTest = File (dirTestPath).createIfNotExists(true)


    val f1 = File(dirTestPath + "/f1").createIfNotExists().overwrite("a")
    val f2 = File(dirTestPath + "/f2").createIfNotExists().overwrite("")

    val diffs = Difference.diffFiles(f1, f2)
    dirTest.delete()
    assert (Diff.findSreen(0, diffs) == Some(List(0, 1)))
  }

  test("findSreen - no screen with write") {
    val dirTestPath = System.getProperty("user.dir") + "/../findScreen";
    val dirTest = File (dirTestPath).createIfNotExists(true)


    val f1 = File(dirTestPath + "/f1").createIfNotExists().overwrite(
      s"""a
        |b
        |c""".stripMargin)
    val f2 = File(dirTestPath + "/f2").createIfNotExists().overwrite(
      s"""a
        |b
        |c""".stripMargin)

    val diffs = Difference.diffFiles(f1, f2)
    dirTest.delete()
    assert (Diff.findSreen(0, diffs) == None)
  }

  test("findSreen - one screen 2") {
    val dirTestPath = System.getProperty("user.dir") + "/../findScreen";
    val dirTest = File (dirTestPath).createIfNotExists(true)


    val f1 = File(dirTestPath + "/f1").createIfNotExists().overwrite(
      s"""a
         |b""".stripMargin)
    val f2 = File(dirTestPath + "/f2").createIfNotExists().overwrite(
      s"""a
         |b
         |c""".stripMargin)

    val diffs = Difference.diffFiles(f1, f2)
    dirTest.delete()
    assert (Diff.findSreen(0, diffs) == Some(List(0, 3)))
  }

  test("findSreen - two screen should return one") {
    val dirTestPath = System.getProperty("user.dir") + "/../findScreen";
    val dirTest = File (dirTestPath).createIfNotExists(true)

    val f1 = File(dirTestPath + "/f1").createIfNotExists().overwrite(
      s"""b
         |c
         |d
         |e
         |f
         |g
         |h
         |i
         |j
         |k
         |l""".stripMargin)
    val f2 = File(dirTestPath + "/f2").createIfNotExists().overwrite(
      s"""a
         |b
         |c
         |d
         |e
         |f
         |g
         |h
         |i
         |j
         |k""".stripMargin)

    val diffs = Difference.diffFiles(f1, f2)
    dirTest.delete()
    assert (Diff.findSreen(0, diffs) == Some(List(0, 3)))
  }

  test("findSreen - diff at the end") {
    val dirTestPath = System.getProperty("user.dir") + "/../findScreen";
    val dirTest = File (dirTestPath).createIfNotExists(true)


    val f1 = File(dirTestPath + "/f1").createIfNotExists().overwrite(
      s"""a
         |b
         |c
         |d
         |e
         |f
         |g
         |h
         |i
         |j
         |k
         |l""".stripMargin)
    val f2 = File(dirTestPath + "/f2").createIfNotExists().overwrite(
      s"""a
         |b
         |c
         |d
         |e
         |f
         |g
         |h
         |i
         |j
         |k""".stripMargin)

    val diffs = Difference.diffFiles(f1, f2)
    dirTest.delete()
    assert (Diff.findSreen(0, diffs) == Some(List(8, 12)))
  }

  test("findSreen - two diffs middle") {
    val dirTestPath = System.getProperty("user.dir") + "/../findScreen";
    val dirTest = File (dirTestPath).createIfNotExists(true)


    val f1 = File(dirTestPath + "/f1").createIfNotExists().overwrite(
      s"""a
         |b
         |c
         |d
         |e
         |h
         |i
         |j
         |k
         |l""".stripMargin)
    val f2 = File(dirTestPath + "/f2").createIfNotExists().overwrite(
      s"""a
         |b
         |c
         |d
         |e
         |f
         |g
         |h
         |i
         |j
         |k
         |l""".stripMargin)


    val diffs = Difference.diffFiles(f1, f2)

    dirTest.delete()
    assert (Diff.findSreen(0, diffs) == Some(List(2,9)))
  }

  test("computeHeading - one diff heading") {

    val dirTestPath = System.getProperty("user.dir") + "/../findScreen";
    val dirTest = File (dirTestPath).createIfNotExists(true)


    val f1 = File(dirTestPath + "/f1").createIfNotExists().overwrite(
      s"""a
         |b
         |c""".stripMargin)
    val f2 = File(dirTestPath + "/f2").createIfNotExists().overwrite(
      s"""a
         |b""".stripMargin)

    val diffs = Difference.diffFiles(f1, f2)
    dirTest.delete()
    assert (Diff.computeHeading(0, 3, diffs) == Console.BLUE + "@@ -1,2 +1,3 @@" + Console.WHITE)


  }

  test("computeHeading - several diff heading") {
    val dirTestPath = System.getProperty("user.dir") + "/../findScreen";
    val dirTest = File (dirTestPath).createIfNotExists(true)


    val f1 = File(dirTestPath + "/f1").createIfNotExists().overwrite(
      s"""a
         |b
         |c
         |d
         |e""".stripMargin)
    val f2 = File(dirTestPath + "/f2").createIfNotExists().overwrite(
      s"""a
         |b""".stripMargin)

    val diffs = Difference.diffFiles(f1, f2)
    dirTest.delete()
    assert (Diff.computeHeading(0, 4, diffs) == Console.BLUE + "@@ -1,2 +1,5 @@" + Console.WHITE)
  }

  /*test("computeDiffString - one diff") {
    val dirTestPath = System.getProperty("user.dir") + "/../findScreen";
    val dirTest = File (dirTestPath).createIfNotExists(true)


    val f1 = File(dirTestPath + "/f1").createIfNotExists().overwrite(
      s"""a
         |b""".stripMargin)
    val f2 = File(dirTestPath + "/f2").createIfNotExists().overwrite(
      s"""a""".stripMargin)

    val diffs = Difference.diffFiles(f1, f2)
    dirTest.delete()
    val exected = Console.BLUE + s"@@ -1,1 +1,2 @@\n" + Console.WHITE + s" a" + Console.WHITE + "\n" + Console.GREEN + s"+b" + Console.WHITE + "\n"
    assert (Diff.computeDiffString(diffs) == exected)
  }

  test("makeDiff - two screen should return one") {
    val dirTestPath = System.getProperty("user.dir") + "/../findScreen";
    val dirTest = File (dirTestPath).createIfNotExists(true)
    Init.init(dirTest)

    val f1 = File(dirTestPath + "/f1").createIfNotExists().overwrite(
      s"""a
         |b
         |c
         |d
         |e
         |f
         |g
         |h
         |i
         |j
         |k
         |l""".stripMargin)

    Add.makeAdd(Seq("f1"), dirTest);
    f1.overwrite(s"""1
                    |a
                    |b
                    |c
                    |d
                    |e
                    |f
                    |g
                    |h
                    |i
                    |j
                    |l""".stripMargin)

    val expected = Console.BLUE + "@@ -1,3 +1,4 @@" + Console.WHITE + "\n"+
      Console.GREEN + "+1" +  Console.WHITE +
       " a" +  Console.WHITE + "\n" +
       " b" +  Console.WHITE + "\n" +
       " c" +  Console.WHITE +" \n" +
      Console.BLUE + "@@ -8,5 +9,4 @@" + Console.WHITE + "\n"+
      " h" +  Console.WHITE + "\n" +
      " i" +  Console.WHITE + "\n" +
      " j" +  Console.WHITE + "\n" +
      Console.RED + "-k" +  Console.WHITE + "\n" +
      " j" +  Console.WHITE + "\n"
      dirTest.delete()
    assert (Diff.makeDiff(dirTest) == expected)
  }*/
}


