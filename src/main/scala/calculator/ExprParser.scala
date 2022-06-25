package calculator
import scala.util.parsing.combinator._

trait ExprParser extends JavaTokenParsers {

  trait Expression
  case class Add(t1: Expression, t2: Expression) extends Expression
  case class Sub(t1: Expression, t2: Expression) extends Expression
  case class Mul(t1: Expression, t2: Expression) extends Expression
  case class Div(t1: Expression, t2: Expression) extends Expression
  case class Num(t: Double) extends Expression

  def eval(t: Expression): Double = t match {
    case Add(t1, t2) => eval(t1)+eval(t2)
    case Sub(t1, t2) => eval(t1)-eval(t2)
    case Mul(t1, t2) => eval(t1)*eval(t2)
    case Div(t1, t2) => eval(t1)/eval(t2)
    case Num(t) => t
  }

  lazy val expr: Parser[Expression] = term ~ rep("[+-]".r ~ term) ^^ {
    case t ~ ts => ts.foldLeft(t) {
      case (t1, "+" ~ t2) => Add(t1, t2)
      case (t1, "-" ~ t2) => Sub(t1, t2)
    }
  }

  lazy val term = factor ~ rep("[*/]".r ~ factor) ^^ {
    case t ~ ts => ts.foldLeft(t) {
      case (t1, "*" ~ t2) => Mul(t1, t2)
      case (t1, "/" ~ t2) => Div(t1, t2)
    }
  }

  lazy val factor = "(" ~> expr <~ ")" | num

  lazy val num = floatingPointNumber ^^ { t => Num(t.toDouble) }
}
