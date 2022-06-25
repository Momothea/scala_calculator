package calculator
import scala.util.parsing.combinator._

class calculator extends RegexParsers with Expr {
  /*
  //characterLiteral ::=  ‘'’ (printableChar | charEscapeSeq) ‘'’
  def variable: Parser[Expr] = "X" ^^ {case ("X") => Var("X")}
  def number: Parser[Double] = ("""\d+(\.\d*)?""".r ^^ { _.toDouble }
  def factor: Parser[Expr] = number | "(" ~> expr <~ ")" //^^ {case _ ~ exprv ~ _ => expr}
  def term  : Parser[Expr] = factor ~ rep( "*" ~ factor | "/" ~ factor) ^^ {
    case number ~ list => (number /: list) {
      case (x, "*" ~ y) => Mul(x , y)
      case (x, "/" ~ y) => Div(x , y)
    }
  }
  def expr  : Parser[Expr] = term ~ rep("+" ~ log(term)("Plus term") | "-" ~ log(term)("Minus term")) ^^ {
    case number ~ list => list.foldLeft(number) { // same as before, using alternate name for /:
      case (x, "+" ~ y) => Add(x , y)
      case (x, "-" ~ y) => Sub(x , y)
    }
  }



  def apply(input: String): Option[Expr] = parseAll(expr, input) match {
    case Success(result, _) => Some(result)
    case failure : NoSuccess => scala.sys.error(failure.msg)
  }*/
}
