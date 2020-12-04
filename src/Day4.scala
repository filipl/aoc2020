import Day4.Passport._

object Day4 extends Base(4) {

  case class Passport(map: Map[String, String]) {
    def hasRequiredFields: Boolean =
      requiredFields.forall(f => map.keys.toList.contains(f))

    private lazy val validHeight =
      map("hgt") match {
        case hgtMatch(height, "cm") => height.toInt >= 150 && height.toInt <= 193
        case hgtMatch(height, "in") => height.toInt >= 59 && height.toInt <= 76
        case _ => false
      }

    def valid: Boolean = {
      lazy val byr = map("byr").toInt
      lazy val iyr = map("iyr").toInt
      lazy val eyr = map("eyr").toInt
      hasRequiredFields &&
        byr >= 1920 &&
        byr <= 2002 &&
        iyr >= 2010 &&
        iyr <= 2020 &&
        eyr >= 2020 &&
        eyr <= 2030 &&
        validHeight &&
        hclMatch.pattern.matcher(map("hcl")).matches() &&
        eclMatch.pattern.matcher(map("ecl")).matches() &&
        pidMatch.pattern.matcher(map("pid")).matches()
    }
  }

  object Passport {
    private val requiredFields = Seq("byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid")
    private val hclMatch = raw"#[0-9a-f]{6}".r
    private val eclMatch = raw"amb|blu|brn|gry|grn|hzl|oth".r
    private val pidMatch = raw"\d{9}".r
    private val hgtMatch = raw"(\d+)(cm|in)".r
    private val passportField = raw"([^:]+):([^ ]+)".r

    def parse(str: String): Passport =
      Passport(str.split(raw"[ \n]").map {
        case passportField(key, value) => key -> value
      }.toMap)
  }

  def passports: Seq[Passport] =
    inputLines
      .mkString("\n")
      .split(raw"\n\n")
      .map(Passport.parse)

  override def first: Int =
    passports.count(_.hasRequiredFields)

  override def second: Int =
    passports.count(_.valid)
}