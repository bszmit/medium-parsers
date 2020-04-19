package pl.bszmit.parsers

import atto.Atto._
import atto._

object TestDrive extends App {

  val Expr: Parser[Double] = {
    val p1 = for {
      lhs <- double
      _   <- char('+')
      rhs <- Expr
    } yield lhs + rhs

    val p2 = double

    p1 | p2
  }

  List(
    Expr.parse("1").done, // Done(,1.0)
    Expr.parse("1+2").done, // Done(,3.0)
    Expr.parse("1+2+3+4+5").done, // Done(,15.0)
  ).foreach(println)
}
