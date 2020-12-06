import scala.io.Source
import scala.util.Try

abstract class Base(day: Int) {
  def readInput(path: String): String = {
    val file = Source.fromFile(path)
    val input = file.mkString
    file.close()
    input
  }

  protected val divider = "\n"

  var input: String = ""
  def inputLines: List[String] = input.split(divider).toList

  def first: Any = "not implemented"
  def second: Any = "not implemented"

  private def readTests(part: String): Seq[(List[String], String)] = {
    Try(readInput(s"input/day$day-test-$part.txt"))
      .getOrElse("")
      .split(divider)
      .foldLeft(List(List[String]())) {
        case (res, "") => List[String]() :: res
        case (cur :: res, l) => (l :: cur) :: res
      }
      .map(_.reverse)
      .flatMap {
        case answer :: lines => Some((lines, answer))
        case _ => None
      }
  }

  def testPart(part: String)(fn: => Any): Boolean =
    readTests(part)
      .map {
        case (testInput, correctAnswer) =>
          input = testInput.mkString(divider)
          val answer = fn.toString
          val correct = answer == correctAnswer
          if (!correct) {
            println(s"INCORRECT TEST\ninput:\n$input\ncorrect:$correctAnswer\nanswer:$answer")
          }
          correct
      }
      .foldLeft(true){
        case (prev, cur) => prev && cur
      }

  def firstTest(): Boolean = testPart("first")(first)
  def secondTest(): Boolean = testPart("second")(second)

  private def printSolution(part: String, solution: Any): Unit = {
    solution match {
      case value: List[_] =>
        println(s"$part solution:")
        value.foreach(println)
      case value: Any =>
        println(s"$part solution: $value")
    }
  }

  def main(args: Array[String]): Unit = {
    firstTest()
    input = readInput(s"input/day$day.txt")
    printSolution("first", first)
    secondTest()
    input = readInput(s"input/day$day.txt")
    printSolution("second", second)
  }
}
