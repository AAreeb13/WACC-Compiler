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
    

    private lazy val parser = fully(prog)
    private lazy val prog = Prog(exprs)

    private lazy val exprs = many(expr)
    
    private lazy val expr: Parsley[Expr] = 
        precedence[Expr](atom)(
            Ops(InfixL)(Add from "+", Sub from "-"),
            Ops(InfixL)(Mul from "*", Mod from "%"), 
            Ops(InfixL)(Div from "/")
        )

    private lazy val atom = Var(ident)
        
}
