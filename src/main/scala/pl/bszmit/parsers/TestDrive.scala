package pl.bszmit.parsers

import atto.Atto._
import atto._

object TestDrive extends App {

  val Expr: Parser[Double] = for {
    lhs <- double
    _   <- char('+')
    rhs <- double
  } yield lhs + rhs

  List(
    Expr.parse("1+2").done, // Done(,3.0)
    Expr.parse("1+2+3").done, // Done(+3,3.0) -- expected 6.0
  ).foreach(println)
}
