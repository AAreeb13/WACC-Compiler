package wacc

import parsley.{Parsley, Result}
import Parsley._
import parsley.combinator._
import parsley.syntax.zipped._
import parsley.syntax.character.stringLift
import parsley.errors.ErrorBuilder
import parsley.expr._
import parsley.debug._
import lexer._


object parser {
    def parse[Err: ErrorBuilder](input: String): Result[Err, Prog] = parser.parse(input)
    
    lazy val parser = fully(prog)
    
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
        = atomic(chain.postfix1(baseType | pairType)(ArrType from "[" <~> "]"))

    lazy val pairType
        = PairType(atomic("pair" ~> "(") ~>  pairElemType, "," ~> pairElemType <~ ")")

    lazy val pairElemType: Parsley[Type] 
        = baseType |
        arrType |
        (ErasedPair from "pair")
    
    lazy val prog = "begin" ~> Prog(many(func), stmtList) <~ "end"


    lazy val exprs = many(expr)
    
    lazy val expr: Parsley[Expr] = 
        precedence(atom)(
            Ops(Prefix)(Not from "!", Neg from "-", Len from atomic("len"), Ord from atomic("ord"), Chr from atomic("chr")),
            Ops(InfixL)(Div from "/", Mul from "*", Mod from "%"), 
            Ops(InfixL)(Add from "+", Sub from "-"),
            Ops(InfixN)(atomic(GrtEql from ">="), Grt from ">", atomic(LessEql from "<="), Less from "<"),
            Ops(InfixN)(Eql from "==", NotEql from "!="),
            Ops(InfixR)(And from "&&"),
            Ops(InfixR)(Or from "||")
        )

    lazy val arrElem = atomic(ArrayVal(ident, some("[" ~> expr <~ "]"))) 

    lazy val atom 
        = arrElem |
        atomic((PairVal from pairLiter)) |
        atomic(BoolVal(boolLiter)) |
        Var(ident) | 
        IntVal(intLiter) | 
        CharVal(charLiter) | 
        StrVal(strLiter)| 
        "(" ~> expr <~ ")"    


    

    lazy val func: Parsley[Func] = Func(types, ident, "(" ~> paramList <~ ")", "is" ~> stmtList <~ "end") 

    lazy val stmtList = sepBy1(stmt, ";")
    lazy val argList = sepBy(expr, ",")
    lazy val arrLiter = ArrLiter(sepBy(expr, ","))

    lazy val stmt: Parsley[Stmt] 
        = (Skip from "skip") |
        atomic(AssignNew(types, ident <~ "=", rvalue)) |
        atomic(Assign(lvalue, "=" ~> rvalue)) |
        atomic(Read("read" ~> lvalue)) |
        Free("free" ~> expr) |
        Return("return" ~> expr) |
        Exit("exit" ~> expr) |
        Print("print" ~> expr) |
        Println("println" ~> expr) |
        If("if" ~> expr, "then" ~> stmtList, "else" ~> stmtList <~ "fi") |
        While("while" ~> expr, "do" ~> stmtList <~ "done") |
        Scope("begin" ~> stmtList <~ "end") 

    lazy val param = Param(types, ident)
    lazy val paramList = sepBy1(param, ",")
    lazy val rvalue 
        = expr | 
        arrLiter |
        NewPair("(" ~> expr <~ ",", expr <~ ")") |
        pairElem |
        FuncCall("call" ~> ident, "(" ~> argList <~ ")")
    
    lazy val lvalue: Parsley[LValue] 
        = Var(ident) |
        arrElem |
        pairElem
    
    lazy val pairElem 
        = Fst("fst" ~> lvalue) | 
        Snd("snd" ~> lvalue)
}