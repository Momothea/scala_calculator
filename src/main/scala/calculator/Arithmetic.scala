package calculator
import scala.util.parsing.combinator._
import scala.util.parsing.combinator.token.StdTokens

class Arithmetic extends RegexParsers  {
  ///characterLiteral ::=  ‘'’ (printableChar | charEscapeSeq) ‘'’

  def variable: Parser[Any] = """X"""
  def expr: Parser[Any] = term~rep("+"~term )
  def term: Parser[Any] = factor~rep("*"~factor | "/"~factor)
  def factor: Parser[Any] = ("""\d+(\.\d*)?""".r ^^ { _.toDouble }) | "("~expr~")" | variable
}
