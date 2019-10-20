package Utils.Difference
import java.nio.file.Paths

import Commands.Init
import better.files.File
import org.scalatest.FunSuite

class DifferenceFilesTest extends FunSuite {
  test ("With same txt") {
    val t1 = Seq("a", "b", "c")
    val t2 = Seq("a", "b", "c");
    val d1 = DifferenceFile(DiffEnum.EQUALS, 0, "a");
    val d2 = DifferenceFile(DiffEnum.EQUALS, 1, "b");
    val d3 = DifferenceFile(DiffEnum.EQUALS, 2, "c");
    val diffs = Seq(d1, d2, d3);
    assert(Difference.diffSeqString(t1, t2) === diffs);
  }

  test ("diffSeqString - With one add at the end") {
    var t1 = Seq("a", "b", "c")
    var t2 = Seq("a", "b", "c", "d");
    val d1 = DifferenceFile(DiffEnum.EQUALS, 0, "a");
    val d2 = DifferenceFile(DiffEnum.EQUALS, 1, "b");
    val d3 = DifferenceFile(DiffEnum.EQUALS, 2, "c");
    var d4 = DifferenceFile(DiffEnum.ADD, 3, "d");
    var diffs = Seq(d1, d2, d3, d4);
    assert(Difference.diffSeqString(t1, t2) === diffs);
  }

  test ("diffSeqString - With a choice to best solution : DELETE") {
    val t1 = Seq("a", "b", "c", "d", "e")
    val t2 = Seq("c", "d", "e", "a", "b");
    val d1 = DifferenceFile(DiffEnum.DELETE, 0, "a");
    val d2 = DifferenceFile(DiffEnum.DELETE, 1, "b");
    val d3 = DifferenceFile(DiffEnum.EQUALS, 2, "c");
    val d4 = DifferenceFile(DiffEnum.EQUALS, 3, "d");
    val d5 = DifferenceFile(DiffEnum.EQUALS, 4, "e");
    val d6 = DifferenceFile(DiffEnum.ADD, 5, "a");
    val d7 = DifferenceFile(DiffEnum.ADD, 6, "b");
    val diffs = Seq(d1, d2, d3, d4, d5, d6, d7);
    assert(Difference.diffSeqString(t1, t2) === diffs);
  }

  test ("diffSeqString - With a choice to best solution : ADD") {
    val t1 = Seq("a", "b", "c", "d", "e")
    val t2 = Seq("d", "e", "a", "b", "c");
    val d1 = DifferenceFile(DiffEnum.ADD, 0, "d");
    val d2 = DifferenceFile(DiffEnum.ADD, 1, "e");
    val d3 = DifferenceFile(DiffEnum.EQUALS, 2, "a");
    val d4 = DifferenceFile(DiffEnum.EQUALS, 3, "b");
    val d5 = DifferenceFile(DiffEnum.EQUALS, 4, "c");
    val d6 = DifferenceFile(DiffEnum.DELETE, 5, "d");
    val d7 = DifferenceFile(DiffEnum.DELETE, 6, "e");
    val diffs = Seq(d1, d2, d3, d4, d5, d6, d7);
    assert(Difference.diffSeqString(t1, t2) === diffs);
  }

}

class DifferenceDirTest extends FunSuite {
  test ("computeDirDiff - Concat two res") {
    val diffs = Difference.computeDirDiff(
      Seq(DifferenceDir("txt1", DiffEnum.DELETE)),
      Seq(DifferenceDir("txt2", DiffEnum.DELETE))
    )
    assert(diffs === Seq(DifferenceDir("txt1", DiffEnum.DELETE), DifferenceDir("txt2", DiffEnum.ADD)))
  }
  test("diffDirectories - With same directory") {
    val dir = System.getProperty("user.dir") + "/../";
    val f = File(dir + "/testDirSGit").createIfNotExists(true)
    File (dir + "/testDirSGit/SA.json").createIfNotExists().overwrite("hello");
    val diffs = Difference.diffDirectories(f, f)
    f.delete()
    assert(diffs === Seq())
  }

  test("diffDirectories - Between two files") {
    val dirTestPath = System.getProperty("user.dir") + "/../testDir";
    val dirTest = File(dirTestPath).createIfNotExists(true)

    val dir1 = File(dirTestPath + "/dir1").createIfNotExists(true)
    val dir2 = File(dirTestPath + "/dir2").createIfNotExists(true)
    val f1 = File(dirTestPath + "/dir1/hello.txt").createIfNotExists().overwrite("hello1")
    val f2 = File(dirTestPath + "/dir2/hello.txt").createIfNotExists().overwrite("hello2")

    val diff = Difference.computeDiffBetweenTwoFiles(dir1, dir2, Paths.get("hello.txt"))

    var diffs : Seq[DifferenceDir] = Seq()
    diff match {
      case Some(diff) => diffs = Seq(DifferenceDir("hello.txt", diff))
      case None =>
    }

    dirTest.delete()


    assert(diffs === Seq(DifferenceDir("hello.txt", DiffEnum.MODIFY)))
  }

  test("diffDirectories - With one diff file") {
    val dirTestPath = System.getProperty("user.dir") + "/../asupp";
    val dirTest = File(dirTestPath).createIfNotExists(true)
    val f1 = File(dirTestPath + "/dir1").createIfNotExists(true)
    val f2 = File(dirTestPath + "/dir2").createIfNotExists(true)
    File(dirTestPath + "/dir1/t1.txt").createIfNotExists().overwrite("Hello")
    File(dirTestPath + "/dir2/t2.txt").createIfNotExists().overwrite("Hello2")
    val diffs = Difference.diffDirectories(f2, f1)

    dirTest.delete()

    assert(diffs === Seq(
      DifferenceDir("t2.txt", DiffEnum.DELETE),
      DifferenceDir("t1.txt", DiffEnum.ADD),
    ))

  }

  test("diffDirectories - With two diff file") {
    val dir = System.getProperty("user.dir") + "/../";

    // Create directories
    val f1 = File(dir + "/dir1").createIfNotExists(true)
    val f2 = File(dir + "/dir2").createIfNotExists(true)

    // Create files
    File(dir + "/dir1/t1.txt").createIfNotExists().overwrite("Hello")
    File(dir + "/dir1/t2.txt").createIfNotExists().overwrite("Hello")
    File(dir + "/dir2/t3.txt").createIfNotExists().overwrite("Hello2")

    val diffs = Difference.diffDirectories(f1, f2)

    f1.delete()
    f2.delete()

    assert(diffs === Seq(
      DifferenceDir("t1.txt", DiffEnum.DELETE),
      DifferenceDir("t2.txt", DiffEnum.DELETE),
      DifferenceDir("t3.txt", DiffEnum.ADD)
    ))

  }

  test("diffDirectories - With one modify file") {
    val dir = System.getProperty("user.dir") + "/../";
    val f1 = File(dir + "/dir1").createIfNotExists(true)
    val f2 = File(dir + "/dir2").createIfNotExists(true)
    File(dir + "/dir1/t1.txt").createIfNotExists().overwrite("Hello")
    File(dir + "/dir2/t1.txt").createIfNotExists().overwrite("Hello2")
    val diffs = Difference.diffDirectories(f1, f2)

    f1.delete()
    f2.delete()

    assert(diffs === Seq(
      DifferenceDir("t1.txt", DiffEnum.MODIFY)
    ))


  }

  test("diffDirectories - With one file in one directory") {
    val dir = System.getProperty("user.dir") + "/../dirTestOnFile";

    // Create directories
    val f1 = File(dir + "/dir1").createIfNotExists(true)
    val f2 = File(dir + "/dir2").createIfNotExists(true)
    File(dir + "/dir1/dir1bis").createIfNotExists(true)

    // Create files
    File(dir + "/dir1/dir1bis/t1.txt").createIfNotExists().overwrite("Hello")
    File(dir + "/dir1/t1.txt").createIfNotExists().overwrite("Hello")
    File(dir + "/dir2/t2.txt").createIfNotExists().overwrite("Hello")

    val diffs = Difference.diffDirectories(f1, f2)

    File(dir).delete()

    assert(diffs === Seq(
      DifferenceDir("dir1bis/t1.txt", DiffEnum.DELETE),
      DifferenceDir("t1.txt", DiffEnum.DELETE),
      DifferenceDir("t2.txt", DiffEnum.ADD)
    ))


  }

  test("diffDirectories - Add a directory and a file") {
    val dir = System.getProperty("user.dir") + "/../";

    // Create directories
    val f1 = File(dir + "/dir1").createIfNotExists(true)
    val f2 = File(dir + "/dir2").createIfNotExists(true)
    File(dir + "/dir1/dir1bis").createIfNotExists(true)

    // Create files
    File(dir + "/dir1/dir1bis/t1.txt").createIfNotExists().overwrite("Hello")
    File(dir + "/dir1/t1.txt").createIfNotExists().overwrite("Hello")
    File(dir + "/dir2/t2.txt").createIfNotExists().overwrite("Hello")

    val diffs = Difference.diffDirectories(f1, f2)

    f1.delete()
    f2.delete()

    assert(diffs === Seq(
      DifferenceDir("dir1bis/t1.txt", DiffEnum.DELETE),
      DifferenceDir("t1.txt", DiffEnum.DELETE),
      DifferenceDir("t2.txt", DiffEnum.ADD)
    ))
  }

  test("diffOfDiffDir - No diff, empty") {
    val diff1 = Seq();
    val diff2 = Seq();

    val diffBetweenDiff  = Difference.diffOfDiffDir(diff1, diff2);
    assert (Seq() === diffBetweenDiff)

  }

  test("diffOfDiffDir - No diff, no empty") {
    val diff1 = Seq(DifferenceDir("dir1bis/t1.txt", DiffEnum.DELETE));
    val diff2 = Seq(DifferenceDir("dir1bis/t1.txt", DiffEnum.DELETE));

    val diffBetweenDiff  = Difference.diffOfDiffDir(diff1, diff2);
    assert (Seq() === diffBetweenDiff)

  }

  test("diffOfDiffDir - One diff") {
    val diff1 = Seq(
      DifferenceDir("dir1bis/t1.txt", DiffEnum.DELETE),
      DifferenceDir("dir1bis/t2.txt", DiffEnum.ADD));
    val diff2 = Seq(DifferenceDir("dir1bis/t1.txt", DiffEnum.DELETE));

    val diffBetweenDiff  = Difference.diffOfDiffDir(diff1, diff2);
    assert (Seq(DifferenceDir("dir1bis/t2.txt", DiffEnum.ADD)) === diffBetweenDiff)
  }

  test("computeDirDiff - Empty Seq") {
    val union = Difference.unionDirDiff(Seq(), Seq());
    assert(union === Seq())
  }

  test("computeDirDiff - Empty Seq and one filled seq") {
    val union = Difference.unionDirDiff(Seq(DifferenceDir("/hello", DiffEnum.ADD)), Seq());
    assert(union === Seq(DifferenceDir("/hello", DiffEnum.ADD)))
  }

  test("computeDirDiff - Two filled seq with one seq in common") {
    val union = Difference.unionDirDiff(
      Seq(DifferenceDir("/hello", DiffEnum.ADD), DifferenceDir("/hello2", DiffEnum.ADD)),
      Seq(DifferenceDir("/hello", DiffEnum.ADD))
    );
    assert(union === Seq(DifferenceDir("/hello2", DiffEnum.ADD), DifferenceDir("/hello", DiffEnum.ADD)))
  }

  test("getDiffOnTop - one diff") {
    val dirTestPath = System.getProperty("user.dir") + "/../diffenceTest";
    val dirTest = File (dirTestPath).createIfNotExists(true)

    val d1 = File(dirTestPath + "/d1").createIfNotExists(true)
    val d2 = File(dirTestPath + "/d2").createIfNotExists(true)

    File(dirTestPath + "/d1/d3/d4").createIfNotExists(true, true)

    val res = Difference.getDiffOnTop(d1, d2)

    dirTest.delete()
    assert(Seq(DifferenceDir("d3/", DiffEnum.ADD)) === res)
  }



}
