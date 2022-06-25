package calculator

import scala.util.parsing.combinator.RegexParsers

class parseur extends RegexParsers{
  def variable: Parser[Any] = """X"""

}
