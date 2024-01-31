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
    lazy val prog = Prog(exprs)

    lazy val exprs = many(expr)
    
    lazy val expr: Parsley[Expr] = 
        precedence[Expr](atom)(
            Ops(Prefix)(Not from "!", Neg from "-", Len from "len", Ord from "ord", Chr from "chr"),
            Ops(InfixL)(Div from "/", Mul from "*", Mod from "%"), 
            Ops(InfixL)(Add from "+", Sub from "-"),
            Ops(InfixN)(Grt from ">", GrtEql from ">=", Less from "<", LessEql from "<="),
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
   
}