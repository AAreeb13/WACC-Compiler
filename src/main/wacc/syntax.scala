
package wacc

import bridges._

object ast {

sealed trait Node {
    val pos: (Int, Int)
}

////////// TYPES ///////////

sealed trait Type extends Node

sealed trait BaseType  extends Type
case class IntType()(val pos: (Int, Int))    extends BaseType {
    override def toString = "IntType"
}
case class CharType()(val pos: (Int, Int))   extends BaseType {
    override def toString = "CharType"
}
case class BoolType()(val pos: (Int, Int))   extends BaseType {
    override def toString = "BoolType"
}
case class StringType()(val pos: (Int, Int)) extends BaseType {
    override def toString = "StringType"
}

case class ArrayType(t: Type)(val pos: (Int, Int)) extends Type
case class PairType(t1: Type, t2: Type)(val pos: (Int, Int)) extends Type
case class ErasedPair()(val pos: (Int, Int)) extends Type

////////// STATEMENTS ///////////

case class Prog(funcs: List[Func], stats: List[Stat])(val pos: (Int, Int)) extends Node

case class Func(retType: Type, name: String, params: List[Param], stats: List[Stat])(val pos: (Int, Int)) extends Node

case class Param(declType: Type, name: String)(val pos: (Int, Int)) extends Node

sealed trait Stat                                                   extends Node
case class Skip()(val pos: (Int, Int))                                                     extends Stat
case class AssignNew(t: Type, ident: String, rvalue: RValue)(val pos: (Int, Int))        extends Stat {
    override def toString = s"AssignNew($t,\"$ident\",$rvalue)"
}
case class Assign(lvalue: LValue, rvalue: RValue)(val pos: (Int, Int))                   extends Stat
case class Read(lvalue: LValue)(val pos: (Int, Int))                                     extends Stat
case class Free(expr: Expr)(val pos: (Int, Int))                                         extends Stat
case class Return(expr: Expr)(val pos: (Int, Int))                                       extends Stat
case class Exit(expr: Expr)(val pos: (Int, Int))                                         extends Stat
case class Print(expr: Expr)(val pos: (Int, Int))                                        extends Stat
case class Println(expr: Expr)(val pos: (Int, Int))                                      extends Stat
case class If(cond: Expr, ifStat: List[Stat], elseStat: List[Stat])(val pos: (Int, Int)) extends Stat
case class While(cond: Expr, stats: List[Stat])(val pos: (Int, Int))                     extends Stat
case class Scope(stats: List[Stat])(val pos: (Int, Int))                                 extends Stat

sealed trait LValue extends Node
sealed trait RValue extends Node

sealed trait PairElem extends LValue with RValue {
    val lvalue: LValue
}

object PairElem {
    def unapply(p: PairElem): Option[LValue] = Some(p.lvalue)
}

case class FstPair(lvalue: LValue)(val pos: (Int, Int)) extends PairElem
case class SndPair(lvalue: LValue)(val pos: (Int, Int)) extends PairElem

case class ArrayLiteral(exprs: List[Expr])(val pos: (Int, Int))           extends RValue
case class PairCons(fst: Expr, snd: Expr)(val pos: (Int, Int))            extends RValue
case class FuncCall(ident: String, args: List[Expr])(val pos: (Int, Int)) extends RValue

////////// EXPRESSIONS ///////////

sealed trait Expr extends RValue

// literals

case class IntVal(x: BigInt)(val pos: (Int, Int)) extends Expr
case class CharVal(x: Char)(val pos: (Int, Int)) extends Expr {
    override def toString = s"CharVal(\'$x\')"
}
case class BoolVal(x: Boolean)(val pos: (Int, Int)) extends Expr
case class StrVal(x: String)(val pos: (Int, Int)) extends Expr {
    override def toString = s"StringVal(\"$x\")"
}
case class PairVal()(val pos: (Int, Int))       extends Expr
case class Var(v: String)(val pos: (Int, Int)) extends Expr with LValue {
    override def toString = s"Var(\"$v\")"
}

case class ArrayVal(v: String, exprs: List[Expr])(val pos: (Int, Int)) extends Expr with LValue

// binary operators

sealed trait BinOp extends Expr {
    val x: Expr
    val y: Expr
}
object BinOp {
    def unapply(op: BinOp): Option[(Expr, Expr)] = Some((op.x, op.y))
}

sealed trait LogicalOp extends BinOp
object LogicalOp {
    def unapply(op: LogicalOp): Option[(Expr, Expr)] = Some((op.x, op.y))
}

sealed trait ArithmeticOp extends BinOp
object ArithmeticOp {
    def unapply(op: ArithmeticOp): Option[(Expr, Expr)] = Some((op.x, op.y))
}

sealed trait EqualityOp extends BinOp
object EqualityOp {
    def unapply(op: EqualityOp): Option[(Expr, Expr)] = Some((op.x, op.y))
}

sealed trait ComparisonOp extends BinOp
object ComparisonOp {
    def unapply(op: ComparisonOp): Option[(Expr, Expr)] = Some((op.x, op.y))
}

case class Mul(x: Expr, y: Expr)(val pos: (Int, Int))     extends ArithmeticOp
case class Div(x: Expr, y: Expr)(val pos: (Int, Int))     extends ArithmeticOp
case class Mod(x: Expr, y: Expr)(val pos: (Int, Int))     extends ArithmeticOp
case class Add(x: Expr, y: Expr)(val pos: (Int, Int))     extends ArithmeticOp
case class Sub(x: Expr, y: Expr)(val pos: (Int, Int))     extends ArithmeticOp

case class Grt(x: Expr, y: Expr)(val pos: (Int, Int))     extends ComparisonOp 
case class GrtEql(x: Expr, y: Expr)(val pos: (Int, Int))  extends ComparisonOp 
case class Less(x: Expr, y: Expr)(val pos: (Int, Int))    extends ComparisonOp 
case class LessEql(x: Expr, y: Expr)(val pos: (Int, Int)) extends ComparisonOp 

case class Eql(x: Expr, y: Expr)(val pos: (Int, Int))     extends EqualityOp
case class NotEql(x: Expr, y: Expr)(val pos: (Int, Int))  extends EqualityOp

case class And(x: Expr, y: Expr)(val pos: (Int, Int))     extends LogicalOp
case class Or(x: Expr, y: Expr)(val pos: (Int, Int))      extends LogicalOp

// unary operators

sealed trait UnOp extends Expr {
    val x: Expr
}
object UnOp {
    def unapply(op: UnOp): Option[Expr] = Some(op.x)
}

case class Not(x: Expr)(val pos: (Int, Int)) extends UnOp
case class Neg(x: Expr)(val pos: (Int, Int)) extends UnOp
case class Len(x: Expr)(val pos: (Int, Int)) extends UnOp
case class Ord(x: Expr)(val pos: (Int, Int)) extends UnOp
case class Chr(x: Expr)(val pos: (Int, Int)) extends UnOp



////////////////////////////////////////////////////////


object IntType extends ParserBridge0[BaseType]
object CharType extends ParserBridge0[BaseType]
object BoolType extends ParserBridge0[BaseType]
object StringType extends ParserBridge0[BaseType]

object ArrayType extends ParserBridge1[Type, Type]
object PairType extends ParserBridge2[Type, Type, Type]
object ErasedPair extends ParserBridge0[Type]

object Prog extends ParserBridge2[List[Func], List[Stat], Prog]
object Func extends ParserBridge4[Type, String, List[Param], List[Stat], Func]
object Param extends ParserBridge2[Type, String, Param]

object Skip      extends ParserBridge0[Stat]
object AssignNew extends ParserBridge3[Type, String, RValue, Stat]
object Assign    extends ParserBridge2[LValue, RValue, Stat]
object Read      extends ParserBridge1[LValue, Stat]
object Free      extends ParserBridge1[Expr, Stat]
object Return    extends ParserBridge1[Expr, Stat]
object Exit      extends ParserBridge1[Expr, Stat]
object Print     extends ParserBridge1[Expr, Stat]
object Println   extends ParserBridge1[Expr, Stat]
object If        extends ParserBridge3[Expr, List[Stat], List[Stat], Stat]
object While     extends ParserBridge2[Expr, List[Stat], Stat]
object Scope     extends ParserBridge1[List[Stat], Stat]

object FstPair      extends ParserBridge1[LValue, PairElem]
object SndPair      extends ParserBridge1[LValue, PairElem]
object ArrayLiteral extends ParserBridge1[List[Expr], RValue]
object PairCons     extends ParserBridge2[Expr, Expr, RValue]
object FuncCall     extends ParserBridge2[String, List[Expr], RValue]

object PairVal extends ParserBridge0[Expr]
object IntVal  extends ParserBridge1[BigInt, Expr]
object CharVal extends ParserBridge1[Char, Expr]
object BoolVal extends ParserBridge1[Boolean, Expr]
object StrVal  extends ParserBridge1[String, Expr]
object Var     extends ParserBridge1[String, Expr with LValue]
object ArrayVal extends ParserBridge2[String, List[Expr], Expr with LValue]

object Mul     extends ParserBridge2[Expr, Expr, Expr]
object Div     extends ParserBridge2[Expr, Expr, Expr]
object Mod     extends ParserBridge2[Expr, Expr, Expr]
object Add     extends ParserBridge2[Expr, Expr, Expr]
object Sub     extends ParserBridge2[Expr, Expr, Expr]
object Grt     extends ParserBridge2[Expr, Expr, Expr]
object GrtEql  extends ParserBridge2[Expr, Expr, Expr]
object Less    extends ParserBridge2[Expr, Expr, Expr]
object LessEql extends ParserBridge2[Expr, Expr, Expr]
object Eql     extends ParserBridge2[Expr, Expr, Expr]
object NotEql  extends ParserBridge2[Expr, Expr, Expr]
object And     extends ParserBridge2[Expr, Expr, Expr]
object Or      extends ParserBridge2[Expr, Expr, Expr]

object Not extends ParserBridge1[Expr, Expr]
object Neg extends ParserBridge1[Expr, Expr]
object Len extends ParserBridge1[Expr, Expr]
object Ord extends ParserBridge1[Expr, Expr]
object Chr extends ParserBridge1[Expr, Expr]

}