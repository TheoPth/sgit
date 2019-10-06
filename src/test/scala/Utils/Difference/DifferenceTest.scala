package Utils.Difference
import better.files.File
import org.scalatest.FunSuite

class DifferenceFilesTest extends FunSuite {
  test ("With same txt") {
    var t1 = Seq("a", "b", "c")
    var t2 = Seq("a", "b", "c");
    var diffs = Seq();
    assert(Difference.diffFiles(t1, t2) === diffs);
  }

  test ("With one add at the end") {
    var t1 = Seq("a", "b", "c")
    var t2 = Seq("a", "b", "c", "d");
    var d1 = DifferenceFile(DiffEnum.ADD, 3, "d");
    var diffs = Seq(d1);
    assert(Difference.diffFiles(t1, t2) === diffs);
  }

  test ("With a choice to best solution : DELETE") {
    var t1 = Seq("a", "b", "c", "d", "e")
    var t2 = Seq("c", "d", "e", "a", "b");
    var d1 = DifferenceFile(DiffEnum.DELETE, 0, "a");
    var d2 = DifferenceFile(DiffEnum.DELETE, 1, "b");
    var d3 = DifferenceFile(DiffEnum.ADD, 5, "a");
    var d4 = DifferenceFile(DiffEnum.ADD, 6, "b");
    var diffs = Seq(d1, d2, d3, d4);
    assert(Difference.diffFiles(t1, t2) === diffs);
  }

  test ("With a choice to best solution : ADD") {
    var t1 = Seq("a", "b", "c", "d", "e")
    var t2 = Seq("d", "e", "a", "b", "c");
    var d1 = DifferenceFile(DiffEnum.ADD, 0, "d");
    var d2 = DifferenceFile(DiffEnum.ADD, 1, "e");
    var d3 = DifferenceFile(DiffEnum.DELETE, 5, "d");
    var d4 = DifferenceFile(DiffEnum.DELETE, 6, "e");
    var diffs = Seq(d1, d2, d3, d4);
    assert(Difference.diffFiles(t1, t2) === diffs);
  }
}

class DifferenceDirTest extends FunSuite {
  test ("Concat two res") {
    val diffs = Difference.concatDirDiff(
      Seq(DifferenceDir("txt1", DiffEnum.DELETE)),
      Seq(DifferenceDir("txt2", DiffEnum.DELETE))
    )
    assert(diffs === Seq(DifferenceDir("txt1", DiffEnum.DELETE), DifferenceDir("txt2", DiffEnum.ADD)))
  }

  test("With same directori") {
    val dir = System.getProperty("user.dir") + "/../";
    val f = File(dir + "/testDirSGit/SA.json").createIfNotExists(true, true)
    val diffs = Difference.diffDirectories(f, f)
    f.delete()
    assert(diffs === Seq())

  }

  test("With one diff file") {
    val dir = System.getProperty("user.dir") + "/../";
    val f1 = File(dir + "/dir1").createIfNotExists(true)
    val f2 = File(dir + "/dir2").createIfNotExists(true)
    File(dir + "/dir1/t1.txt").createIfNotExists().overwrite("Hello")
    File(dir + "/dir2/t2.txt").createIfNotExists().overwrite("Hello2")
    val diffs = Difference.diffDirectories(f2, f1)

    f1.delete()
    f2.delete()

    assert(diffs === Seq(
      DifferenceDir("t2.txt", DiffEnum.DELETE),
      DifferenceDir("t1.txt", DiffEnum.ADD),
    ))

  }

  test("With two diff file") {
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

  test("With one modify file") {
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

  test("With one file in one directory") {
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

  test("Add a directory and a file") {
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
}
