package wacc

import parsley.generic

sealed trait Node

////////// TYPES ///////////

sealed trait Type extends Node

sealed trait BaseType  extends Type
case object IntType    extends BaseType with generic.ParserBridge0[BaseType]
case object CharType   extends BaseType with generic.ParserBridge0[BaseType]
case object BoolType   extends BaseType with generic.ParserBridge0[BaseType]
case object StringType extends BaseType with generic.ParserBridge0[BaseType]

case class ArrayType(t: Type) extends Type
object ArrayType              extends generic.ParserBridge1[Type, Type]

case class PairType(t1: Type, t2: Type) extends Type
object PairType                         extends generic.ParserBridge2[Type, Type, Type]
case object ErasedPair                  extends Type with generic.ParserBridge0[Type]

////////// STATEMENTS ///////////

case class Prog(funcs: List[Func], stats: List[Stat]) extends Node
object Prog                                           extends generic.ParserBridge2[List[Func], List[Stat], Prog]

case class Func(retType: Type, name: String, params: List[Param], stats: List[Stat]) extends Node
object Func extends generic.ParserBridge4[Type, String, List[Param], List[Stat], Func]

case class Param(declType: Type, name: String) extends Node
object Param                                   extends generic.ParserBridge2[Type, String, Param]

sealed trait Stat                                                   extends Node
case object Skip                                                    extends Stat with generic.ParserBridge0[Stat]
case class AssignNew(t: Type, ident: String, rvalue: RValue)        extends Stat
case class Assign(lvalue: LValue, rvalue: RValue)                   extends Stat
case class Read(lvalue: LValue)                                     extends Stat
case class Free(expr: Expr)                                         extends Stat
case class Return(expr: Expr)                                       extends Stat
case class Exit(expr: Expr)                                         extends Stat
case class Print(expr: Expr)                                        extends Stat
case class Println(expr: Expr)                                      extends Stat
case class If(cond: Expr, ifStat: List[Stat], elseStat: List[Stat]) extends Stat
case class While(cond: Expr, stats: List[Stat])                     extends Stat
case class Scope(stats: List[Stat])                                 extends Stat

object AssignNew extends generic.ParserBridge3[Type, String, RValue, Stat]
object Assign    extends generic.ParserBridge2[LValue, RValue, Stat]
object Read      extends generic.ParserBridge1[LValue, Stat]
object Free      extends generic.ParserBridge1[Expr, Stat]
object Return    extends generic.ParserBridge1[Expr, Stat]
object Exit      extends generic.ParserBridge1[Expr, Stat]
object Print     extends generic.ParserBridge1[Expr, Stat]
object Println   extends generic.ParserBridge1[Expr, Stat]
object If        extends generic.ParserBridge3[Expr, List[Stat], List[Stat], Stat]
object While     extends generic.ParserBridge2[Expr, List[Stat], Stat]
object Scope     extends generic.ParserBridge1[List[Stat], Stat]

sealed trait LValue extends Node
sealed trait RValue extends Node

sealed trait PairElem              extends LValue with RValue
case class FstPair(lvalue: LValue) extends PairElem
case class SndPair(lvalue: LValue) extends PairElem

case class ArrayLiteral(exprs: List[Expr])           extends RValue
case class PairCons(fst: Expr, snd: Expr)            extends RValue
case class FuncCall(ident: String, args: List[Expr]) extends RValue

object FstPair      extends generic.ParserBridge1[LValue, PairElem]
object SndPair      extends generic.ParserBridge1[LValue, PairElem]
object ArrayLiteral extends generic.ParserBridge1[List[Expr], RValue]
object PairCons     extends generic.ParserBridge2[Expr, Expr, RValue]
object FuncCall     extends generic.ParserBridge2[String, List[Expr], RValue]

////////// EXPRESSIONS ///////////

sealed trait Expr extends RValue

// literals

case class IntVal(x: BigInt) extends Expr
case class CharVal(x: Char) extends Expr {
    override def toString = s"CharVal(\'$x\')"
}
case class BoolVal(x: Boolean) extends Expr
case class StrVal(x: String) extends Expr {
    override def toString = s"StringVal(\"$x\")"
}
case object PairVal       extends Expr with generic.ParserBridge0[Expr]
case class Var(v: String) extends Expr with LValue

object IntVal  extends generic.ParserBridge1[BigInt, Expr]
object CharVal extends generic.ParserBridge1[Char, Expr]
object BoolVal extends generic.ParserBridge1[Boolean, Expr]
object StrVal  extends generic.ParserBridge1[String, Expr]
object Var     extends generic.ParserBridge1[String, Expr with LValue]

case class ArrayVal(v: String, exprs: List[Expr]) extends Expr with LValue
object ArrayVal                                   extends generic.ParserBridge2[String, List[Expr], Expr with LValue]

// binary operators

case class Mul(x: Expr, y: Expr)     extends Expr
case class Div(x: Expr, y: Expr)     extends Expr
case class Mod(x: Expr, y: Expr)     extends Expr
case class Add(x: Expr, y: Expr)     extends Expr
case class Sub(x: Expr, y: Expr)     extends Expr
case class Grt(x: Expr, y: Expr)     extends Expr
case class GrtEql(x: Expr, y: Expr)  extends Expr
case class Less(x: Expr, y: Expr)    extends Expr
case class LessEql(x: Expr, y: Expr) extends Expr
case class Eql(x: Expr, y: Expr)     extends Expr
case class NotEql(x: Expr, y: Expr)  extends Expr
case class And(x: Expr, y: Expr)     extends Expr
case class Or(x: Expr, y: Expr)      extends Expr

object Mul     extends generic.ParserBridge2[Expr, Expr, Expr]
object Div     extends generic.ParserBridge2[Expr, Expr, Expr]
object Mod     extends generic.ParserBridge2[Expr, Expr, Expr]
object Add     extends generic.ParserBridge2[Expr, Expr, Expr]
object Sub     extends generic.ParserBridge2[Expr, Expr, Expr]
object Grt     extends generic.ParserBridge2[Expr, Expr, Expr]
object GrtEql  extends generic.ParserBridge2[Expr, Expr, Expr]
object Less    extends generic.ParserBridge2[Expr, Expr, Expr]
object LessEql extends generic.ParserBridge2[Expr, Expr, Expr]
object Eql     extends generic.ParserBridge2[Expr, Expr, Expr]
object NotEql  extends generic.ParserBridge2[Expr, Expr, Expr]
object And     extends generic.ParserBridge2[Expr, Expr, Expr]
object Or      extends generic.ParserBridge2[Expr, Expr, Expr]

// unary operators

case class Not(x: Expr) extends Expr
case class Neg(x: Expr) extends Expr
case class Len(x: Expr) extends Expr
case class Ord(x: Expr) extends Expr
case class Chr(x: Expr) extends Expr

object Not extends generic.ParserBridge1[Expr, Expr]
object Neg extends generic.ParserBridge1[Expr, Expr]
object Len extends generic.ParserBridge1[Expr, Expr]
object Ord extends generic.ParserBridge1[Expr, Expr]
object Chr extends generic.ParserBridge1[Expr, Expr]
