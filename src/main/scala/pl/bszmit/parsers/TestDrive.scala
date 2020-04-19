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
    val p2 = for {
      lhs <- Term
      _   <- char('-')
      rhs <- Expr
    } yield lhs - rhs
    val p3 = Term
    p1 | p2 | p3
  }

  lazy val Term: Parser[Double] = {
    val p1 = for {
      lhs <- double
      _   <- char('*')
      rhs <- Term
    } yield lhs * rhs
    val p2 = for {
      lhs <- double
      _   <- char('/')
      rhs <- Term
    } yield lhs / rhs
    val p3 = double
    p1 | p2 | p3
  }

  List(
    Expr.parse("5-2").done, // Done(,3.0)
    Expr.parse("6-12/2").done, // Done(,0.0)
    Expr.parse("1-2-3").done, // Done(,2.0) -- expected -4.0
    Expr.parse("16/4/2").done, // Done(,8.0) -- expected 2.0
  ).foreach(println)
}
