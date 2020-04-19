package pl.bszmit.parsers

import atto.Atto._
import atto._

object TestDrive extends App {

  lazy val Expr: Parser[Double] = delay {
    val p1 = for {
      lhs <- Expr
      _   <- char('+')
      rhs <- Term
    } yield lhs + rhs
    val p2 = for {
      lhs <- Expr
      _   <- char('-')
      rhs <- Term
    } yield lhs - rhs
    val p3 = Term
    p1 | p2 | p3
  }

  lazy val Term: Parser[Double] = delay {
    val p1 = for {
      lhs <- Term
      _   <- char('*')
      rhs <- double
    } yield lhs * rhs
    val p2 = for {
      lhs <- Term
      _   <- char('/')
      rhs <- double
    } yield lhs / rhs
    val p3 = double
    p1 | p2 | p3
  }

  List(
    Expr.parse("1-2-3").done, // it never ends...
    Expr.parse("1").done, // it never ends...
  ).foreach(println)
}
