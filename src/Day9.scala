import scala.annotation.tailrec

object Day9 extends Base(9) {
  def numbers: List[Long] = inputLines.map(_.toLong)

  def sums(ls: List[Long]): Iterator[Long] =
    ls.combinations(2).map(_.sum)

  def findFirstWithoutSum(ls: List[Long], n: Int): Long = {
    ls.sliding(n + 1).filter { nums =>
      val allSums = sums(nums.take(n))
      !allSums.contains(nums.last)
    }.take(1).toList.head.last
  }

  override def firstTest: Long =
    findFirstWithoutSum(numbers, 5)

  override def first: Long =
    findFirstWithoutSum(numbers, 25)

  def findContiguousSum(ls: List[Long], num: Long): List[Long] = {
    @tailrec
    def findSum(window: Int): List[Long] =
      ls.sliding(window).find(a => a.sum == num) match {
        case Some(l) => l
        case _ => findSum(window + 1)
      }
    findSum(2)
  }

  def weakness(n: Int): Long = {
    val faulty = findFirstWithoutSum(numbers, n)
    val ls = findContiguousSum(numbers, faulty)
    ls.max + ls.min
  }

  override def secondTest: Long = weakness(5)

  override def second: Long = weakness(25)
}