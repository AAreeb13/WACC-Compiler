package wacc

import parsley.generic

case class Prog(funcs: List[Func], stmts: List[Stmt])
object Prog extends generic.ParserBridge2[List[Func], List[Stmt], Prog]

sealed trait Expr extends RValue
sealed trait Type
sealed trait Stmt
sealed trait LValue
sealed trait RValue
sealed trait PairElem extends LValue with RValue

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
case class Var(v: String) extends Expr with LValue
object Var extends generic.ParserBridge1[String, Expr with LValue]

case class IntVal(i: BigInt) extends Expr
object IntVal extends generic.ParserBridge1[BigInt, Expr]

case class CharVal(c: Char) extends Expr
object CharVal extends generic.ParserBridge1[Char, Expr]

case class StrVal(s: String) extends Expr
object StrVal extends generic.ParserBridge1[String, Expr]

case class BoolVal(b: Boolean) extends Expr
object BoolVal extends generic.ParserBridge1[Boolean, Expr]

case object PairVal extends Expr with generic.ParserBridge0[Expr]

case class ArrayVal(id: String, exprs: List[Expr]) extends Expr with LValue
object ArrayVal extends generic.ParserBridge2[String, List[Expr], Expr with LValue]

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

/*------------------------------ Statements ------------------------------*/

case class Func(retType: Type, name: String, params: List[Param], stats: List[Stmt])
object Func extends generic.ParserBridge4[Type, String, List[Param], List[Stmt], Func]

case class Param(_type: Type, name: String)
object Param extends generic.ParserBridge2[Type, String, Param]

case object Skip extends Stmt with generic.ParserBridge0[Stmt]

case class AssignNew(t: Type, name: String, _rvalue: RValue) extends Stmt
object AssignNew extends generic.ParserBridge3[Type, String, RValue, Stmt]

case class Assign(_lvalue: LValue, _rvalue: RValue) extends Stmt
object Assign extends generic.ParserBridge2[LValue, RValue, Stmt]

case class Read(_lvalue: LValue) extends Stmt
object Read extends generic.ParserBridge1[LValue, Stmt]

case class Free(_expr: Expr) extends Stmt
object Free extends generic.ParserBridge1[Expr, Stmt]

case class Return(_expr: Expr) extends Stmt
object Return extends generic.ParserBridge1[Expr, Stmt]

case class Exit(_expr: Expr) extends Stmt
object Exit extends generic.ParserBridge1[Expr, Stmt]

case class Print(expr: Expr) extends Stmt
object Print extends generic.ParserBridge1[Expr, Stmt]

case class Println(expr: Expr) extends Stmt
object Println extends generic.ParserBridge1[Expr, Stmt]

case class If(cond: Expr, ifStat: List[Stmt], elseStat: List[Stmt]) extends Stmt
object If extends generic.ParserBridge3[Expr, List[Stmt], List[Stmt], Stmt]

case class While(cond: Expr, stats: List[Stmt]) extends Stmt
object While extends generic.ParserBridge2[Expr, List[Stmt], Stmt]

case class Scope(stats: List[Stmt]) extends Stmt
object Scope extends generic.ParserBridge1[List[Stmt], Stmt]

case class Fst(_lvalue: LValue) extends PairElem
object Fst extends generic.ParserBridge1[LValue, PairElem]

case class Snd(_rvalue: LValue) extends PairElem
object Snd extends generic.ParserBridge1[LValue, PairElem]

case class NewPair(_expr1: Expr, _expr2: Expr) extends RValue
object NewPair extends generic.ParserBridge2[Expr, Expr, RValue]

case class FuncCall(name: String, args: List[Expr]) extends RValue
object FuncCall extends generic.ParserBridge2[String, List[Expr], RValue]

case class ArrLiter(elems: List[Expr]) extends RValue
object ArrLiter extends generic.ParserBridge1[List[Expr], RValue]