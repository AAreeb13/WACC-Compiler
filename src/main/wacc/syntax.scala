
package wacc

import parsley.generic

sealed trait Node

////////// TYPES ///////////

sealed trait Type extends Node {
    def reducesTo(otherType: Type): Boolean = (this, otherType) match {
        case (NoneType, _) | (_, NoneType) => false
        case (AnyType, _) | (_, AnyType) => true
        case (IntType, IntType) | 
             (CharType, CharType) | 
             (BoolType, BoolType) | 
             (StringType, StringType) => true

        case (ArrayType(a1), ArrayType(a2)) => a1 == a2
        case (ArrayType(CharType), StringType) => true

        case (PairType(AnyType, AnyType), PairType(l1, r1)) => true
        case (PairType(AnyType, r1), PairType(l2, r2)) => r1 == r2
        case (PairType(l1, AnyType), PairType(l2, r2)) => l1 == l2

        case (PairType(l1, r1), PairType(AnyType, AnyType)) => true
        case (PairType(l1, r1), PairType(AnyType, r2)) => r1 == r2
        case (PairType(l1, r1), PairType(l2, AnyType)) => l1 == l2

        case (PairType(l1, r1), PairType(l2, r2)) => (l1 == l2) && (r1 == r2)
        case (ErasedPair, PairType(_, _)) | (PairType(_, _), ErasedPair) => true

        case (ErasedPair, ErasedPair) => false  // last bullet point on pair coercion
        case _ => false
    }
}

sealed trait BaseType  extends Type
case object IntType    extends BaseType with generic.ParserBridge0[BaseType] {
    override def toString = "int"
}
case object CharType   extends BaseType with generic.ParserBridge0[BaseType] {
    override def toString = "char"
}
case object BoolType   extends BaseType with generic.ParserBridge0[BaseType] {
    override def toString = "bool"
}
case object StringType extends BaseType with generic.ParserBridge0[BaseType] {
    override def toString = "string"
}

case class ArrayType(t: Type) extends Type {
    override def toString = s"${t.toString}[]"

    def dimensions: Int = t match {
        case arrType: ArrayType => 1 + arrType.dimensions
        case _ => 1
    }

    def unfold(levels: Int): Option[Type] = (levels, t) match {
        case (1, t) => Some(t)
        case (n, arr: ArrayType) => arr.unfold(n-1)
        case _ => None
    }
}

object ArrayType              extends generic.ParserBridge1[Type, Type]

case class PairType(t1: Type, t2: Type) extends Type  {
    override def toString = s"pair(${t1.toString}, ${t2.toString})"

}
object PairType extends generic.ParserBridge2[Type, Type, Type]

case object ErasedPair extends Type with generic.ParserBridge0[Type]  {
    override def toString = "pair"
}

case object AnyType extends BaseType

case object NoneType extends BaseType

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

sealed trait PairElem extends LValue with RValue {
    val lvalue: LValue
}

object PairElem {
    def unapply(p: PairElem): Option[LValue] = Some(p.lvalue)
}

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

case class Mul(x: Expr, y: Expr)     extends ArithmeticOp
case class Div(x: Expr, y: Expr)     extends ArithmeticOp
case class Mod(x: Expr, y: Expr)     extends ArithmeticOp
case class Add(x: Expr, y: Expr)     extends ArithmeticOp
case class Sub(x: Expr, y: Expr)     extends ArithmeticOp

case class Grt(x: Expr, y: Expr)     extends ComparisonOp 
case class GrtEql(x: Expr, y: Expr)  extends ComparisonOp 
case class Less(x: Expr, y: Expr)    extends ComparisonOp 
case class LessEql(x: Expr, y: Expr) extends ComparisonOp 

case class Eql(x: Expr, y: Expr)     extends EqualityOp
case class NotEql(x: Expr, y: Expr)  extends EqualityOp

case class And(x: Expr, y: Expr)     extends LogicalOp
case class Or(x: Expr, y: Expr)      extends LogicalOp

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

sealed trait UnOp extends Expr {
    val x: Expr
}
object UnOp {
    def unapply(op: UnOp): Option[Expr] = Some(op.x)
}

case class Not(x: Expr) extends UnOp
case class Neg(x: Expr) extends UnOp
case class Len(x: Expr) extends UnOp
case class Ord(x: Expr) extends UnOp
case class Chr(x: Expr) extends UnOp

object Not extends generic.ParserBridge1[Expr, Expr]
object Neg extends generic.ParserBridge1[Expr, Expr]
object Len extends generic.ParserBridge1[Expr, Expr]
object Ord extends generic.ParserBridge1[Expr, Expr]
object Chr extends generic.ParserBridge1[Expr, Expr]
