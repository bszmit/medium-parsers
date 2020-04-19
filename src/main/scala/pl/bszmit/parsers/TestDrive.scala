package pl.bszmit.parsers

import atto.Atto._
import atto._

object TestDrive extends App {

  lazy val Expr: Parser[Double] = for {
    lhs  <- Term
    next <- Expr_prim
  } yield next(lhs)

  lazy val Expr_prim: Parser[Double => Double] = {
    val p1 = for {
      _    <- char('+')
      rhs  <- Term
      next <- Expr_prim
    } yield (lhs: Double) => next(lhs + rhs)
    val p2 = for {
      _    <- char('-')
      rhs  <- Term
      next <- Expr_prim
    } yield (lhs: Double) => next(lhs - rhs)
    val p3 = ok((lhs: Double) => lhs)
    p1 | p2 | p3
  }

  lazy val Term: Parser[Double] = for {
    lhs  <- Factor
    next <- Term_prim
  } yield next(lhs)

  lazy val Term_prim: Parser[Double => Double] = {
    val p1 = for {
      _    <- char('*')
      rhs  <- Factor
      next <- Term_prim
    } yield (lhs: Double) => next(lhs * rhs)
    val p2 = for {
      _    <- char('/')
      rhs  <- Factor
      next <- Term_prim
    } yield (lhs: Double) => next(lhs / rhs)
    val p3 = ok((lhs: Double) => lhs)
    p1 | p2 | p3
  }

  lazy val Factor: Parser[Double] = {
    val p1 = for {
      _ <- char('(')
      e <- Expr
      _ <- char(')')
    } yield e
    val p2 = double
    p1 | p2
  }

  List(
    Expr.parse("1").done, // Done(,1.0)
    Expr.parse("1-2-3").done, // Done(,-4.0)
    Expr.parse("16/4/2").done, // Done(,2.0)
    Expr.parse("1+2+12/4-3*3-2-1").done, // Done(,-6.0)
    Expr.parse("(10-(2+2)*2)/2").done, // Done(,1.0)
    Expr.parse("10-(2+2*2)/2").done, // Done(,7.0)
    Expr.parse("((2+2))*2").done, // Done(,8.0)
  ).foreach(println)
}
