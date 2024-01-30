package wacc

import parsley.generic

case class Prog(exprs: List[Expr])
object Prog extends generic.ParserBridge1[List[Expr], Prog]

sealed trait Expr
case class Add(x: Expr, y: Expr) extends Expr
object Add extends generic.ParserBridge2[Expr, Expr, Expr]

case class Sub(x: Expr, y: Expr) extends Expr
object Sub extends generic.ParserBridge2[Expr, Expr, Expr]

case class Mul(x: Expr, y: Expr) extends Expr
object Mul extends generic.ParserBridge2[Expr, Expr, Expr]

case class Div(x: Expr, y:Expr) extends Expr
object Div extends generic.ParserBridge2[Expr, Expr, Expr]

case class Mod(x: Expr, y: Expr) extends Expr
object Mod extends generic.ParserBridge2[Expr, Expr, Expr]

case class Var(v: String) extends Expr
object Var extends generic.ParserBridge1[String, Expr]