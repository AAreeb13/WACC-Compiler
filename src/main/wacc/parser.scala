package wacc

import parsley.{Parsley, Result}
import Parsley._
import parsley.combinator._
import parsley.syntax.zipped._
import parsley.syntax.character.stringLift
import parsley.errors.ErrorBuilder
import parsley.expr._
import lexer._


object parser {
    def parse[Err: ErrorBuilder](input: String): Result[Err, Prog] = parser.parse(input)
    

    lazy val parser = fully(prog)
    lazy val prog = "begin" ~> Prog(many(func), stmtList) <~ "end"

    lazy val exprs = many(expr)
    
    lazy val expr: Parsley[Expr] = 
        precedence[Expr](atom)(
            Ops(Prefix)(Not from "!", Neg from "-", Len from "len", Ord from "ord", Chr from "chr"),
            Ops(InfixL)(Div from "/", Mul from "*", Mod from "%"), 
            Ops(InfixL)(Add from "+", Sub from "-"),
            Ops(InfixN)(atomic(GrtEql from ">="), Grt from ">", atomic(LessEql from "<="), Less from "<"),
            Ops(InfixN)(Eql from "==", NotEql from "!="),
            Ops(InfixR)(And from "&&"),
            Ops(InfixR)(Or from "||")
        )

    lazy val atom 
        = atomic(ArrayVal(ident, some("[" ~> expr <~ "]"))) |
        atomic((PairVal from pairLiter)) |
        atomic(BoolVal(boolLiter)) |
        Var(ident) | 
        IntVal(intLiter) | 
        CharVal(charLiter) | 
        StrVal(strLiter)| 
        "(" ~> expr <~ ")"    

    lazy val types 
        = baseType |
        arrType |
        pairType
    
    lazy val baseType 
        = (IntType from "int") |
        (CharType from "char") |
        (StrType from "string") |
        (BoolType from "bool")

    lazy val arrType: Parsley[Type]
        = precedence[Type](baseType, pairType)(
            Ops(Postfix)(ArrType from "[" <~> "]")
        )

    lazy val pairType
        = PairType(("pair" ~> "(") ~>  pairElemType, "," ~> pairElemType <~ ")")

    lazy val pairElemType: Parsley[Type] 
        = baseType |
        arrType |
        (ErasedPair from "pair")

    lazy val func: Parsley[Func] = Func(types, ident, "(" ~> paramList <~ ")", "is" ~> stmtList <~ "end") 

    lazy val stmtList = sepBy1(stmt, ";")

    lazy val stmt: Parsley[Stmt] 
        = Skip from "skip" |
        AssignNew(types, ident <~ "=", rvalue) |
        Assign(lvalue, "=" ~> rvalue) |
        Read("read" ~> lvalue) |
        Free("free" ~> expr) |
        Return("return" ~> expr) |
        Exit("exit" ~> expr) |
        Print("print" ~> expr) |
        Println("println" ~> expr) |
        If("if" ~> expr, "then" ~> stmtList, "else" ~> stmtList <~ "fi")
        While("while" ~> expr, "do" ~> stmtList <~ "done") |
        Scope("begin" ~> stmtList <~ "end") 

    lazy val param = Param(types, ident)
    lazy val paramList = sepBy1(param, ",")
    lazy val rvalue = ???
    lazy val lvalue = ???

    lazy val pairElem 
        = Fst("fst" ~> lvalue) | 
        Snd("snd" ~> lvalue)
}