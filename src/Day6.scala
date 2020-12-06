object Day6 extends Base(6) {

  case class Group(s: Seq[String]) {
    def yeses: Int =
      s.flatMap(i => i.getBytes).toSet.size

    def commonYeses: Int =
      s.map(_.getBytes.toSet).reduce {
        (common, el) => common.intersect(el)
      }.size
  }

  def groups: Seq[Group] =
    inputLines
      .mkString("\n")
      .split(raw"\n\n")
      .map(group => Group(group.split(raw"\n"))).toSeq

  override def first: Int =
    groups.map(_.yeses).sum

  override def second: Int =
    groups.map(_.commonYeses).sum
}