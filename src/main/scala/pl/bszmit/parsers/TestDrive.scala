package pl.bszmit.parsers

import atto.Atto._
import atto._

object TestDrive extends App {

  val Expr: Parser[Double] = {
    val p1 = for {
      lhs <- double
      _   <- char('*')
      rhs <- Expr
    } yield lhs * rhs

    val p2 = for {
      lhs <- double
      _   <- char('+')
      rhs <- Expr
    } yield lhs + rhs

    val p3 = double

    p1 | p2 | p3
  }

  List(
    Expr.parse("2+2*2+2").done, // Done(,10.0) -- expected 8
  ).foreach(println)
}
