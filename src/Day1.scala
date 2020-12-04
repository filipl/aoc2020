object Day1 extends Base(1) {
  def numbers: Seq[Int] = inputLines.map(_.toInt)

  private def comb(n: Int) =
    numbers
      .combinations(n)
      .filter(_.sum == 2020)
      .map(_.product)
      .take(1)
      .toSeq
      .head

  override def first: Int = comb(2)
  override def second: Int = comb(3)
}
