package edu.colorado.csci3155.project1

/*--
  TODO : COMPLETED
 ---*/

sealed trait CalcProgram
case class TopLevel(listOfCmds: List[Cmd]) extends CalcProgram
sealed trait Cmd
sealed trait Expr

// .. The rest of the definitions below ...

//Cmd trait
case class Define(s: String, ex: Expr) extends Cmd
case class Display(ex: Expr) extends Cmd

// Expr trait
case class Const(d: Double) extends Expr
case class Ident(s: String) extends Expr
case class Plus(ex1: Expr, ex2: Expr) extends Expr
case class Minus(ex1: Expr, ex2: Expr) extends Expr
case class Mult(ex1: Expr, ex2: Expr) extends Expr
case class Div(ex1: Expr, ex2: Expr) extends Expr
case class Geq(ex1: Expr, ex2: Expr) extends Expr
case class Gt(ex1: Expr, ex2: Expr) extends Expr
case class Eq(ex1: Expr, ex2: Expr) extends Expr
case class And(ex1: Expr, ex2: Expr) extends Expr
case class Or(ex1: Expr, ex2: Expr) extends Expr
case class Not(ex: Expr) extends Expr
case class IfThenElse(ex1: Expr, ex2: Expr, ex3: Expr) extends Expr