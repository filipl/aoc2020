object Day5 extends Base(5) {

  sealed trait Choice
  case object Upper extends Choice
  case object Lower extends Choice

  case class BoardingPass(row: Int, column: Int) {
    def id: Int = row * 8 + column
  }

  object BoardingPass {
    def parse(s: String): BoardingPass = {
      val row = s.take(7).map {
        case 'F' => Lower
        case 'B' => Upper
      }
      val column = s.takeRight(3).map {
        case 'L' => Lower
        case 'R' => Upper
      }

      def calc(range: List[Int], choices: Seq[Choice]): Int =
        choices.foldLeft(range) {
          case (range, Lower) => range.take(range.length / 2)
          case (range, Upper) => range.takeRight(range.length / 2)
        }.head

      BoardingPass(calc((0 to 127).toList, row), calc((0 to 7).toList, column))
    }
  }

  def passes: Seq[BoardingPass] =
    inputLines.map(BoardingPass.parse)

  override def first: Int =
    passes.maxBy(_.id).id

  override def second: Int = {
    val occupiedIds = passes.map(_.id).sorted
    (1 * 8 + 7 to 126 * 8 + 7)
      .filter {
        i => !occupiedIds.contains(i) && occupiedIds.contains(i - 1) && occupiedIds.contains(i + 1)
      }.head
  }
}