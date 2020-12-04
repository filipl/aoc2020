object Day2 extends Base(2) {
  private val password = raw"(\d+)-(\d+) ([^:]+): (.*)".r

  def parsePassword(line: String): Password =
    line match {
      case password(min, max, char, pw) => Password(pw, Policy(min.toInt, max.toInt, char.head))
    }

  private def allPasswords: Seq[Password] = inputLines.map(parsePassword)

  case class Policy(min: Int, max: Int, char: Char)

  case class Password(s: String, policy: Policy) {
    def isValidFirst: Boolean = {
      val c = s.count(c => policy.char == c)
      policy.min <= c && policy.max >= c
    }

    def isValidSecond: Boolean = {
      def validChar(index: Int): Boolean = {
        s.charAt(index - 1) == policy.char
      }
      val fst = validChar(policy.min)
      val snd = validChar(policy.max)
      (fst && !snd) || (!fst && snd)
    }
  }
  
  override def first: Int = allPasswords.count(_.isValidFirst)
  override def second: Int = allPasswords.count(_.isValidSecond)
}