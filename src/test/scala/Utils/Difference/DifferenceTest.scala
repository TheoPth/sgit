package Utils.Difference
import org.scalatest.FunSuite

class DifferenceTest extends FunSuite {
  test ("With same txt") {
    var t1 = Seq("a", "b", "c")
    var t2 = Seq("a", "b", "c");
    var diffs = Seq();
    assert(Difference.diff(t1, t2) === diffs);
  }

  test ("With one add at the end") {
    var t1 = Seq("a", "b", "c")
    var t2 = Seq("a", "b", "c", "d");
    var d1 = Difference(DiffEnum.ADD, 3, "d");
    var diffs = Seq(d1);
    assert(Difference.diff(t1, t2) === diffs);
  }

  test ("With a choice to best solution : DELETE") {
    var t1 = Seq("a", "b", "c", "d", "e")
    var t2 = Seq("c", "d", "e", "a", "b");
    var d1 = Difference(DiffEnum.DELETE, 0, "a");
    var d2 = Difference(DiffEnum.DELETE, 1, "b");
    var d3 = Difference(DiffEnum.ADD, 5, "a");
    var d4 = Difference(DiffEnum.ADD, 6, "b");
    var diffs = Seq(d1, d2, d3, d4);
    assert(Difference.diff(t1, t2) === diffs);
  }

  test ("With a choice to best solution : ADD") {
    var t1 = Seq("a", "b", "c", "d", "e")
    var t2 = Seq("d", "e", "a", "b", "c");
    var d1 = Difference(DiffEnum.ADD, 0, "d");
    var d2 = Difference(DiffEnum.ADD, 1, "e");
    var d3 = Difference(DiffEnum.DELETE, 5, "d");
    var d4 = Difference(DiffEnum.DELETE, 6, "e");
    var diffs = Seq(d1, d2, d3, d4);
    assert(Difference.diff(t1, t2) === diffs);
  }
}
