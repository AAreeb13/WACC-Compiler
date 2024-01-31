package wacc

import parsley.generic

case class Prog(exprs: List[Expr])
object Prog extends generic.ParserBridge1[List[Expr], Prog]

sealed trait Expr
sealed trait Type

/*------------------------------ Binary Operators ------------------------------*/

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

case class Grt(x: Expr, y: Expr) extends Expr
object Grt extends generic.ParserBridge2[Expr, Expr, Expr]

case class GrtEql(x: Expr, y: Expr) extends Expr
object GrtEql extends generic.ParserBridge2[Expr, Expr, Expr]

case class Less(x: Expr, y: Expr) extends Expr
object Less extends generic.ParserBridge2[Expr, Expr, Expr]

case class LessEql(x: Expr, y: Expr) extends Expr
object LessEql extends generic.ParserBridge2[Expr, Expr, Expr]

case class Eql(x: Expr, y: Expr) extends Expr
object Eql extends generic.ParserBridge2[Expr, Expr, Expr]

case class NotEql(x: Expr, y: Expr) extends Expr
object NotEql extends generic.ParserBridge2[Expr, Expr, Expr]

case class And(x: Expr, y: Expr) extends Expr
object And extends generic.ParserBridge2[Expr, Expr, Expr]

case class Or(x: Expr, y: Expr) extends Expr
object Or extends generic.ParserBridge2[Expr, Expr, Expr]

/*------------------------------ Atoms ------------------------------*/

/* For <ident> */
case class Var(v: String) extends Expr
object Var extends generic.ParserBridge1[String, Expr]

case class IntVal(i: BigInt) extends Expr
object IntVal extends generic.ParserBridge1[BigInt, Expr]

case class CharVal(c: Char) extends Expr
object CharVal extends generic.ParserBridge1[Char, Expr]

case class StrVal(s: String) extends Expr
object StrVal extends generic.ParserBridge1[String, Expr]

case class BoolVal(b: Boolean) extends Expr
object BoolVal extends generic.ParserBridge1[Boolean, Expr]

case object PairVal extends Expr with generic.ParserBridge0[Expr]

case class ArrayVal(id: String, exprs: List[Expr]) extends Expr
object ArrayVal extends generic.ParserBridge2[String, List[Expr], Expr]

/*------------------------------ Unary Operators ------------------------------*/

case class Not(x: Expr) extends Expr
object Not extends generic.ParserBridge1[Expr, Expr]

case class Neg(x: Expr) extends Expr
object Neg extends generic.ParserBridge1[Expr, Expr]

case class Len(x: Expr) extends Expr
object Len extends generic.ParserBridge1[Expr, Expr]

case class Ord(x: Expr) extends Expr
object Ord extends generic.ParserBridge1[Expr, Expr]

case class Chr(x: Expr) extends Expr
object Chr extends generic.ParserBridge1[Expr, Expr]

/*------------------------------ Types ------------------------------*/

case object IntType extends Type with generic.ParserBridge0[Type]
case object CharType extends Type with generic.ParserBridge0[Type]
case object StrType extends Type with generic.ParserBridge0[Type]
case object BoolType extends Type with generic.ParserBridge0[Type]

case class ArrType(t: Type) extends Type
object ArrType extends generic.ParserBridge1[Type, Type]

case class PairType(t1: Type, t2: Type) extends Type
object PairType extends generic.ParserBridge2[Type, Type, Type]

case object ErasedPair extends Type with generic.ParserBridge0[Type]