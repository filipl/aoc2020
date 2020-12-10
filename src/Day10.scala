object Day10 extends Base(10) {
  def adaptors: Seq[Int] = inputLines.map(_.toInt).sorted

  def diffs(prev: Int, adaptors: Seq[Int]): List[Int] = {
    adaptors match {
      case Nil => List(3)
      case a :: rest => a - prev :: diffs(a, rest)
    }
  }

  override def first: Int = {
    val ds = diffs(0, adaptors)
    ds.count(_ == 1) * ds.count(_ == 3)
  }

  def combinations(adaptors: Seq[Int], cache: Map[Int,Long]): Map[Int,Long] = {
    adaptors match {
      case a :: _ if cache.contains(a) => cache
      case a :: Nil => Map(a -> 1L) ++ cache
      case a :: rest =>
        val reachable = rest.takeWhile(b => b - a <= 3)
        val updatedCache = reachable.foldLeft(cache) { (c, r) =>
          combinations(r :: rest.dropWhile(_ <= r), c)
        }
        val subs = reachable.map(r => updatedCache(r)).sum
        Map(a -> subs) ++ updatedCache
    }
  }

  override def second: Long =
    combinations(0 :: adaptors.toList, Map())(0)
}