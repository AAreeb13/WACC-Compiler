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
            Ops(InfixL)(Div from "/", Mul from "*", Mod from "%"), 
            Ops(InfixL)(Add from "+", Sub from "-")
        )

    lazy val atom 
        = Var(ident) | 
        IntVal(intLiter) | 
        CharVal(charLiter) | 
        StrVal(strLiter)| 
        BoolVal(boolLiter) |
        (PairVal from pairLiter) | 
        ArrayVal(ident, some("[" ~> expr <~ "]")) |
        "(" ~> expr <~ ")"       
}