package calculator


object mainApp extends  ExpressionParser   {
  def main(args: Array[String]): Unit = {


    /*
    println("input : "+ "1 + (2 + 1) + X")
    var a = (parseAll(expr, "1 + (2 + 1) + X"))
    println(a)
    println(a.getClass)
    println(showExpr(Mul(Num(4), Var("x"))))
    println(eval(Num(4))(4))
    */

    val e = "1"
    println(e)

    val input = "6"
    println("input : ", input)
    var a = (parseAll(expr, input))
    //println((a.get))

    val b = a.get

    println(showExpr(b))

    println(eval(b)(3))


    /*
    val e = Expr(10)
    val expression = Expr match {
      case Expr() => println("There is nothing")
      case Expr(number,_,_,_) =>
    }
    */

  }

}
