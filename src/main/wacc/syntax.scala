
package wacc

import bridges._
import semanticChecker.FuncInfo

/**
  * This file is concerned with the literal syntactical AST types. They are
  * generated during syntax analysis and then piped in semantic analysis and thus
  * the rest of the pipeline
  */

object ast {
    /**
     * AST Node superclass. Everything is a node and thus has some form of position
     * information and also a reference to its current scope (symbol table)
     */
    sealed trait Node {
        val pos: (Int, Int)
        var scope: SymbolTable = null
    }

    /*--------------------------------------- Types ---------------------------------------*/

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
    case class ErasedPair()(val pos: (Int, Int)) extends Type {
        override def toString = "ErasedPair"
    }

    /*--------------------------------------- Statements ---------------------------------------*/

    case class Prog(funcs: List[Func], stats: List[Stat])(val pos: (Int, Int)) extends Node


    case class Func(retType: Type, name: String, params: List[Param], stats: List[Stat])(val pos: (Int, Int)) extends Node {
        override def toString = s"Func($retType,\"$name\",$params,$stats)"
    }

    case class Param(declType: Type, name: String)(val pos: (Int, Int)) extends Node with Declarable {
        override def toString = s"Param($declType,\"$name\")"
    }

    /**
     * Used to filter out statements that don't end in a return, used for functions 
     */
    def containsReturn(statList: List[Stat]): Boolean = statList.lastOption match {
        case None => false
        case Some(Return(_)) | Some(Exit(_)) => true
        case Some(If(_, ifStats, elseStats)) => containsReturn(ifStats) && containsReturn(elseStats)
        case Some(While(_, stats)) => containsReturn(stats)
        case Some(Scope(stats)) => containsReturn(stats)
        case _ => false
    }

    sealed trait TypeCapture {
        var enclosingType: semAst.SemType = null
    }

    sealed trait Printable extends TypeCapture {
        val expr: Expr
    }

    object Printable {
        def unapply(p: Printable): Option[Expr] = Some(p.expr)
    }

    sealed trait Declarable {
        val declType: Type
        val name: String
    }

    sealed trait Stat                                                   extends Node
    case class Skip()(val pos: (Int, Int))                                                   extends Stat {
        override def toString = "Skip"
    }

    case class AssignNew(declType: Type, name: String, rvalue: RValue)(val pos: (Int, Int))        extends Stat with Declarable {
        override def toString = s"AssignNew($declType,\"$name\",$rvalue)"
    }
    
    case class Assign(lvalue: LValue, rvalue: RValue)(val pos: (Int, Int))                   extends Stat
    case class Read(lvalue: LValue)(val pos: (Int, Int))                                     extends Stat with TypeCapture
    case class Free(expr: Expr)(val pos: (Int, Int))                                         extends Stat
    case class Return(expr: Expr)(val pos: (Int, Int))                                       extends Stat
    case class Exit(expr: Expr)(val pos: (Int, Int))                                         extends Stat
    case class Print(expr: Expr)(val pos: (Int, Int))                                        extends Stat with Printable
    case class Println(expr: Expr)(val pos: (Int, Int))                                      extends Stat with Printable
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

    case class ArrayLiteral(exprs: List[Expr])(val pos: (Int, Int))           extends RValue with TypeCapture
    case class PairCons(fst: Expr, snd: Expr)(val pos: (Int, Int))            extends RValue
    case class FuncCall(ident: String, args: List[Expr])(val pos: (Int, Int)) extends RValue {
        var funcInfo: FuncInfo = null
        override def toString = s"FuncCall(\"$ident\",$args)"
    }

    /*--------------------------------------- Expressions ---------------------------------------*/

    sealed trait Expr extends RValue

        /*----------------------------------- Literals -----------------------------------*/

    case class IntVal(x: BigInt)(val pos: (Int, Int)) extends Expr
    case class CharVal(x: Char)(val pos: (Int, Int)) extends Expr {
        override def toString = s"CharVal(\'$x\')"
    }
    case class BoolVal(x: Boolean)(val pos: (Int, Int)) extends Expr
    case class StrVal(x: String)(val pos: (Int, Int)) extends Expr {
        override def toString = s"StrVal(\"$x\")"
    }
    case class PairVal()(val pos: (Int, Int))       extends Expr {
        override def toString = "PairVal"
    }
    case class Var(v: String)(val pos: (Int, Int)) extends Expr with LValue {
        override def toString = s"Var(\"$v\")"
    }

    case class ArrayVal(v: String, exprs: List[Expr])(val pos: (Int, Int)) extends Expr with LValue with TypeCapture {
        override def toString = s"ArrayVal(\"$v\",$exprs)"
    }

        /*------------------------------- Binary Operators -------------------------------*/

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

    sealed trait ComparativeOp extends BinOp
    object ComparativeOp {
        def unapply(op: ComparativeOp): Option[(Expr, Expr)] = Some((op.x, op.y))
    }

    sealed trait EqualityOp extends ComparativeOp
    object EqualityOp {
        def unapply(op: EqualityOp): Option[(Expr, Expr)] = Some((op.x, op.y))
    }

    sealed trait ComparisonOp extends ComparativeOp
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

        /*-------------------------------- Unary Operators --------------------------------*/

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

    /*--------------------------------------- Companion Objects for above case classes ---------------------------------------*/

    /**
     * These are all the companion objects for all AST nodes, this allows position
     * information to be hooked in and also override the labels for errors
     */

    object IntType extends ParserBridge0[BaseType] {
        override def labels = List{"type"}
    }
    object CharType extends ParserBridge0[BaseType] {
        override def labels = List{"type"}
    }
    object BoolType extends ParserBridge0[BaseType] {
        override def labels = List{"type"}
    }
    object StringType extends ParserBridge0[BaseType] {
        override def labels = List{"type"}
    }

    object ArrayType extends ParserBridge1[Type, Type] {
        override def labels = List{"type"}
    }
    object PairType extends ParserBridge2[Type, Type, Type] {
        override def labels = List{"type"}
    }
    object ErasedPair extends ParserBridge0[Type] {
        override def labels = List{"type"}
    }

    object Prog extends ParserBridge2[List[Func], List[Stat], Prog]
    object Func extends ParserBridge3[(Type, String), List[Param], List[Stat], Func] {
        def apply(tuple: (Type, String), params: List[Param], stats: List[Stat])(pos: (Int, Int) = (0, 0)) = tuple match {
            case (retType, name) => Func(retType, name, params, stats)(pos)
        }
    }
    object Param extends ParserBridge2[Type, String, Param]

    object Skip      extends ParserBridge0[Stat]
    object AssignNew extends ParserBridge3[Type, String, RValue, Stat]
    object Assign    extends ParserBridge2[LValue, RValue, Stat] {
        override def labels = List{"assignment"}
    }
    object Read      extends ParserBridge1[LValue, Stat]  {
        override def labels = List{"read"}
    }
    object Free      extends ParserBridge1[Expr, Stat]
    object Return    extends ParserBridge1[Expr, Stat] {
        override def labels = List{"return"}
    }
    object Exit      extends ParserBridge1[Expr, Stat]
    object Print     extends ParserBridge1[Expr, Stat]
    object Println   extends ParserBridge1[Expr, Stat]
    object If        extends ParserBridge3[Expr, List[Stat], List[Stat], Stat] {
        override def labels = List{"if"}
    }
    object While     extends ParserBridge2[Expr, List[Stat], Stat] {
        override def labels = List{"while"}
    }
    object Scope     extends ParserBridge1[List[Stat], Stat] {
        override def labels = List{"new scope"}
        override def reason = Some("all program body and function declarations must be within `begin` and `end`")
    }

    object FstPair      extends ParserBridge1[LValue, PairElem] {
        override def labels = List("fst")
    }
    object SndPair      extends ParserBridge1[LValue, PairElem] {
        override def labels = List("snd")
    }
    object ArrayLiteral extends ParserBridge1[List[Expr], RValue] {
        override def labels = List{"array literal"}
    }
    object PairCons     extends ParserBridge2[Expr, Expr, RValue] 
    object FuncCall     extends ParserBridge2[String, List[Expr], RValue] {
        override def labels = List{"function call"}
    }

    object PairVal extends ParserBridge0[Expr] {
        override def labels = List("pair literal")
    }
    object IntVal  extends ParserBridge1[BigInt, Expr] {
        override def labels = List("expression")
        override def reason = Some("expressions may start with integer, string, character or boolean literals; identifiers; unary operators; null; or parentheses\n" +
        "in addition, expressions may contain array indexing operations; and comparison, logical, and arithmetic operators")
    }
    object CharVal extends ParserBridge1[Char, Expr] {
        override def labels = List("expression")
        override def reason = Some("expressions may start with integer, string, character or boolean literals; identifiers; unary operators; null; or parentheses\n" +
        "in addition, expressions may contain array indexing operations; and comparison, logical, and arithmetic operators")
    }
    object BoolVal extends ParserBridge1[Boolean, Expr] {
        override def labels = List("expression")
        override def reason = Some("expressions may start with integer, string, character or boolean literals; identifiers; unary operators; null; or parentheses\n" +
        "in addition, expressions may contain array indexing operations; and comparison, logical, and arithmetic operators")
    }
    object StrVal  extends ParserBridge1[String, Expr] {
        override def labels = List("expression")
        override def reason = Some("expressions may start with integer, string, character or boolean literals; identifiers; unary operators; null; or parentheses\n" +
        "in addition, expressions may contain array indexing operations; and comparison, logical, and arithmetic operators")
    }
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

    /* Disambiguation bridge for variables and array literals */

    object VarOrArrayVal extends ParserBridge2[String, List[Expr], Expr with LValue] {
        /* If exprs is a non-empty list then we parse an array literal, otherwise it's a variable */
        def apply(_expr: String, exprs: List[Expr])(pos: (Int, Int) = (0, 0)): Expr with LValue = exprs match {
            case head :: next => ArrayVal(_expr, exprs)(pos)
            case Nil => Var(_expr)(pos)
        }
        override def labels = List{"array literal"}
    }

}
