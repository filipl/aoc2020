import scala.annotation.tailrec

object Day7 extends Base(7) {

  case class Rule(bag: String, contains: Map[String,Int])

  val bagMatch = raw"(\d) (\w+ \w+)".r

  object Rule {
    def parse(s: String): Rule = {
      val aoeu = s.split(" bags contain ")
      val bagName = aoeu.head
      val contains = aoeu.drop(1).head
        .replace(".", "")
        .split(", ")
        .flatMap { c =>
          bagMatch.findAllMatchIn(c).map(m => m.group(2) -> m.group(1).toInt)
        }.toMap
      Rule(bagName, contains)
    }
  }

  def rules: Seq[Rule] = inputLines.map(Rule.parse)

  def contains: Map[String,List[String]] =
    rules.foldLeft(Map[String,List[String]]()) {
      (cs, rule) =>
        rule.contains.keys.foldLeft(cs) { (c, bag) =>
          val current = c.getOrElse(bag, List()) :+ rule.bag
          c ++ Map(bag -> current)
        }
    }

  def countUniqueBagsWithin(bag: String)(implicit contains: Map[String,List[String]]): Set[String] = {
    val directContains = contains.getOrElse(bag, Set()).toSet
    directContains ++ directContains.flatMap(countUniqueBagsWithin)
  }

  def countNeededBags(bag: String)(implicit rules: Map[String,Rule]): Int =
    1 + rules(bag).contains.map {
      case (innerBag, amount) =>
        amount * countNeededBags(innerBag)
    }.sum

  override def first: Int =
    countUniqueBagsWithin("shiny gold")(contains).size

  override def second: Int =
    countNeededBags("shiny gold")(rules.map(r => r.bag -> r).toMap) - 1
}