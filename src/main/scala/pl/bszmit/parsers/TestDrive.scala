package pl.bszmit.parsers

import atto.Atto._
import atto._

object TestDrive extends App {

  lazy val Expr: Parser[Double] = {
    val p1 = for {
      lhs <- Term
      _   <- char('+')
      rhs <- Expr
    } yield lhs + rhs
    val p2 = Term

    p1 | p2
  }

  lazy val Term: Parser[Double] = {
    val p1 = for {
      lhs <- double
      _   <- char('*')
      rhs <- Term
    } yield lhs * rhs
    val p2 = double
    p1 | p2
  }

  List(
    Expr.parse("2+2*2").done, // Done(,6.0)
    Expr.parse("2*2+2").done, // Done(,6.0)
    Expr.parse("2*2+3*2").done, // Done(,10.0)
    Expr.parse("2+2*2+2").done, // Done(,8.0)
  ).foreach(println)
}
