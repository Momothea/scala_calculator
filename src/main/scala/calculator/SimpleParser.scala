package calculator
import scala.util.parsing.combinator._


class SimpleParser extends RegexParsers {
  def word: Parser[String]   = """[a-z]+""".r       ^^ { _.toString }
  def numberr: Parser[Int]    = """(0|[1-9]\d*)""".r ^^ { _.toInt }
  def freq: Parser[WordFreq] = word ~ numberr        ^^ { case wd ~ fr => WordFreq(wd,fr) }
}