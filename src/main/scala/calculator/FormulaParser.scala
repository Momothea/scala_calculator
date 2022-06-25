package calculator
import scala.math._
import scala.util.parsing.combinator._
import scala.util.Random

class FormulaParser(val variables: Set[String] = Set(),
                    val constants: Map[String, Double] = Map(),
                    val unary: Map[String, Double => Double] = Map(),
                    val binary: Map[String, (Double, Double) => Double] = Map(),
                    val userFcts: Map[String, String => Double] = Map(),
                    random: Random = new Random) extends JavaTokenParsers {
  require(constants.keySet.intersect(userFcts.keySet).isEmpty)
  val allConstants = constants ++ Map("E" -> E, "PI" -> Pi, "Pi" -> Pi)
  // shouldn´t be empty
  val unaryOps = Map[String, Double => Double](
    "sqrt" -> (sqrt(_)), "abs" -> (abs(_)), "floor" -> (floor(_)), "ceil" -> (ceil(_)), "ln" -> (math.log(_)), "round" -> (round(_).toDouble), "signum" -> (signum(_))
  ) ++ unary
  val binaryOps1 = Map[String, (Double, Double) => Double](
    "+" -> (_ + _), "-" -> (_ - _), "*" -> (_ * _), "/" -> (_ / _), "^" -> (pow(_, _))
  )
  val binaryOps2 = Map[String, (Double, Double) => Double](
    "max" -> (max(_, _)), "min" -> (min(_, _))
  ) ++ binary

  type Argument = Map[String, Double]
  type Formula = Argument => Double

  def fold(d: Formula, l: List[~[String, Formula]]) = l.foldLeft(d) { case (d1, op ~ d2) => arg => binaryOps1(op)(d1(arg), d2(arg))}
  implicit def set2Parser[V](s: Set[String]) = s.map(_ ^^ identity).reduceLeft(_ | _)
  implicit def map2Parser[V](m: Map[String, V]) = m.keys.map(_ ^^ identity).reduceLeft(_ | _)
  def expression: Parser[Formula] = sign ~ term ~ rep(("+" | "-") ~ term) ^^ { case s ~ t ~ l => fold(arg => s * t(arg), l)}
  def sign: Parser[Double] = opt("+" | "-") ^^ { case None => 1; case Some("+") => 1; case Some("-") => -1}
  def term: Parser[Formula] = longFactor ~ rep(("*" | "/") ~ longFactor) ^^ { case d ~ l => fold(d, l)}
  def longFactor: Parser[Formula] = shortFactor ~ rep("^" ~ shortFactor) ^^ { case d ~ l => fold(d, l)}
  def shortFactor: Parser[Formula] = fpn | sign ~ (constant | variable | rnd | unaryFct | binaryFct | userFct | "(" ~> expression <~ ")") ^^ { case s ~ x => arg => s * x(arg)}
  def constant: Parser[Formula] = allConstants ^^ (name => arg => allConstants(name))
  def variable: Parser[Formula] = variables ^^ (name => arg => arg(name))
  def rnd: Parser[Formula] = "rnd" ~> "(" ~> fpn ~ "," ~ fpn <~ ")" ^^ { case x ~ _ ~ y => (arg: Argument) => require(y(arg) > x(arg)); x(arg) + (y(arg) - x(arg)) * random.nextDouble} | "rnd" ^^ { _ => arg => random.nextDouble}
  def fpn: Parser[Formula] = floatingPointNumber ^^ (value => arg => value.toDouble)
  def unaryFct: Parser[Formula] = unaryOps ~ "(" ~ expression ~ ")" ^^ { case op ~ _ ~ d ~ _ => arg => unaryOps(op)(d(arg))}
  def binaryFct: Parser[Formula] = binaryOps2 ~ "(" ~ expression ~ "," ~ expression ~ ")" ^^ { case op ~ _ ~ d1 ~ _ ~ d2 ~ _ => arg => binaryOps2(op)(d1(arg), d2(arg))}
  def userFct: Parser[Formula] = userFcts ~ "(" ~ (expression ^^ (_.toString) | ident) <~ ")" ^^ { case fct ~ _ ~ x => arg => userFcts(fct)(x)}
  def evaluate(formula: String) = parseAll(expression, formula).get
}