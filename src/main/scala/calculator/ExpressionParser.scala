package calculator
import scala.util.parsing.combinator._


class ExpressionParser extends RegexParsers with Expr  {

  def operand: Parser[Expr] = (number | variable )
  def variable: Parser[Expr] = "x" ^^ { _ => X }
  def number: Parser[Expr] = """-?\d+""".r ^^ { s => Num(s.toDouble) }

  // functions

  //def expression = operation1
  def expr = operation1
  def operation1: Parser[Expr] = operation2 ~ rep("+" ~ operation2) ^^ {
    case op ~ list => list.foldLeft(op) {
      case (x, "+" ~ y) => Add(x, y)
    }
  }
  def operation2: Parser[Expr] = factor ~ rep("*" ~ factor) ^^ {
    case op ~ list => list.foldLeft(op) {
      case (x, "*" ~ y) => Mul(x, y)
    }
  }

  //def funct: Parser[Expr] = sin | cos



  def sin: Parser[Expr] = rep("sin(" ~ factor ~ ")")  ^^ {
    case list => list.foldLeft[Expr](Num(0)){
      case (c , "sin(" ~ x~")") => Sin(x)
    }
  }
/*
  def sin: Parser[Expr] = rep("sin" ~ factor)  ^^ {
    case list => list.foldRight(){
      case ("sin" ~ x , y) => Sin(x)
    }
  }
  */

  //def cos: Parser[Expr] = "cos" ~ "("~expr~")"

  // expression

  def factor: Parser[Expr] = operand | /* funct |*/ "("~ expr ~")" ^^ {case _ ~expr~ _ => expr}


  def apply(input: String): Option[Expr] = parseAll(expr, input) match {
    case Success(result, _) => Some(result)
    case NoSuccess(_, _) => None
  }

}
