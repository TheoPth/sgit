package Utils.Difference

case class Difference(diff: DiffEnum.Value, index: Int, content: String) {}

object Difference {
  /*
  Compute the delta between two files. Delta is modifications to apply to text 1 to get text 2.
  One difference between files is stored as Difference (class)
   */
  def diff (text1: Seq[String], text2: Seq[String]) : Seq[Difference] = {
    def aux(t1: Seq[String], t2: Seq[String], diffs: Seq[Difference], index: Int) : Seq[Difference] = {
      // Is both empty, all differences have been computed
      if (t1.isEmpty && t2.isEmpty) {
        return diffs
      }

      if (t1.isEmpty) {
        return aux(t1, t2.tail, diffs :+ Difference(DiffEnum.ADD, index, t2.head), index + 1)
      }

      if (t2.isEmpty) {
        return aux(t1.tail, t2, diffs :+ Difference(DiffEnum.DELETE, index, t1.head), index + 1)
      }


      if (t1.head.equals(t2.head)) {
        return aux(t1.tail, t2.tail, diffs, index + 1)
      } else {
        // Id not equals, we can get delete text 1 line or add text 2 line. We cannot know what the vest choice is
        // So try both and keep the better one (solution with less diff)
        var pos1 = aux(t1, t2.tail, diffs :+ Difference(DiffEnum.ADD, index, t2.head), index + 1)
        var pos2 = aux(t1.tail, t2, diffs :+ Difference(DiffEnum.DELETE, index, t1.head), index + 1)
        if (pos1.length < pos2.length) pos1 else pos2
      }
    }

    return aux(text1, text2, Seq(), 0);
  }
}