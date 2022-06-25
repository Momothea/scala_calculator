package calculator
//sealed trait correctExpression
import scala.util.parsing.combinator._



trait Expr   {
  case class Add(t1: Expr, t2: Expr) extends Expr

  case class Sub(t1: Expr, t2: Expr) extends Expr

  case class Mul(t1: Expr, t2: Expr) extends Expr

  case class Div(t1: Expr, t2: Expr) extends Expr

  case class Num(t: Double) extends Expr

  case class Sin(t : Expr) extends  Expr

  case class Cos(t : Expr) extends  Expr

  case object X extends Expr

  //case class Var(t: String) extends Expr // I am not really sure of this case


  def showExpr( e : Expr): String = {
    e match{
      case Num(t) => s"$t"
      case X => s"X"
      case Add(t1: Expr, t2: Expr) => s"${ showExpr(t1)} + ${showExpr(t2)} "
      case Mul(t1: Expr, t2: Expr) => s"${ showExpr(t1)} * ${showExpr(t2)} "
      case Div(t1: Expr, t2: Expr) => s"${ showExpr(t1)} / ${showExpr(t2)} "
      case Sub(t1: Expr, t2: Expr) => s"${ showExpr(t1)} - ${showExpr(t2)} "

    }
  }

  def eval(t: Expr) (x: Double): Double = t match {
    case Num(t) => t
    case X => x
    case Add(t1: Expr, t2: Expr) => eval(t1)(x)  + eval(t2)(x)
    case Mul(t1: Expr, t2: Expr) => eval(t1)(x)  * eval(t2)(x)
  }

}
