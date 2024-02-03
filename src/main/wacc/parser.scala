package wacc

import parsley.Parsley, Parsley._
import parsley.combinator._
import parsley.syntax.zipped._
import parsley.expr.chain
import parsley.errors.ErrorBuilder
import parsley.debug._
import parsley.character.digit

import lexer._
import lexer.implicits.implicitSymbol
import parsley.expr.Ops
import parsley.expr.{InfixL, InfixN, InfixR, Prefix, Postfix}
import parsley.expr.precedence
import parsley.errors.combinator._
import parsley.Result

object parser {
    // the Err: ErrorBuilder here is saying that the compiler must be able to find a value
    // of type ErrorBuilder[Err] to provide implicitly to this function. When you use this
    // in the REPL, it will default to ErrorBuilder[String]. In part 3, the tests will instantiate
    // it differently
    // If you like, you can think of it as having type:
    //def parse(input: String): Either[String, Prog]
    def parse[Err: ErrorBuilder](input: String): Result[Err, Node]  = parser.parse(input)
    //def parse[Err: ErrorBuilder](input: String) = parser.parse(input)

    lazy val parser: Parsley[Node] = fully(prog)

    ////////// TYPE PARSER ///////////
    lazy val declType: Parsley[Type]
        = arrayType | 
        pairType | 
        baseType

    lazy val baseType: Parsley[BaseType]
        = (IntType from "int") | 
        (StringType from "string") | 
        (CharType from "char") | 
        (BoolType from "bool")

    lazy val arrayType: Parsley[Type]
        //= precedence(baseType, pairType)(Ops(Postfix)(ArrayType from "[" <~> "]"))
        = atomic(chain.postfix1(baseType | pairType)(ArrayType from "[" <~> "]"))
    
    lazy val pairType
        = PairType(atomic("pair" ~> "(") ~> pairElemType, "," ~> pairElemType <~ ")")

    lazy val pairElemType
        = arrayType | 
        baseType | 
        (ErasedPair from "pair")

    // probably don't need the lastOption since grammar guarantees that
    // all filtered stmtLists are valid and contains at least one valid during filter
    // but just to be on the safe side
    private def containsReturn(stmtOption: Option[Stat]): Boolean = stmtOption match {
        case None => false
        case Some(stmt) => stmt match {
            case Return(expr) => true
            case Exit(expr) => true
            case If(cond, ifstmts, elsestmts) => containsReturn(ifstmts.lastOption) && containsReturn(elsestmts.lastOption)
            case While(cond, stmts) => containsReturn(stmts.lastOption)
            case Scope(stmts) => containsReturn(stmts.lastOption)
            case _ => false
        }
    }
        
    

    ////////// stmt PARSER ///////////

    lazy val prog = "begin" ~> Prog(funcList, stmtList) <~ "end"
    lazy val funcList = many(func)
    lazy val func 
        = atomic(Func(declType, ident, "(" ~> paramList <~ ")", "is" ~> stmtList.filter(stmts => containsReturn(stmts.lastOption)) <~ "end"))

    lazy val paramList = sepBy(param, ",")
    lazy val param = Param(declType, ident)

    lazy val stmtList: Parsley[List[Stat]] = sepBy1(stmt, ";")

    lazy val stmt
        = "skip".as(Skip) | 
        AssignNew(declType, ident <~ "=", rvalue) | 
        Assign(atomic(lvalue <~ "="), rvalue) | 
        Read("read" ~> lvalue) | 
        Free("free" ~> expr) | 
        Return("return" ~> expr) | 
        Exit("exit" ~> expr) | 
        Print("print" ~> expr) | 
        Println("println" ~> expr) | 
        If("if" ~> expr, "then" ~> stmtList, "else" ~> stmtList <~ "fi") | 
        While("while" ~> expr, "do" ~> stmtList <~ "done") | 
        Scope("begin" ~> stmtList <~ "end") 

    lazy val lvalue: Parsley[LValue]
        = arrayElem | 
        Var(ident) | 
        pairElem
    
    lazy val rvalue: Parsley[RValue]
        = expr | 
        arrayLiteral | 
        PairCons("newpair" ~> "(" ~> expr, "," ~> expr <~ ")") | 
        pairElem | 
        FuncCall("call" ~> ident, "(" ~> argList <~ ")")
        
    lazy val argList = sepBy(expr, ",")
    lazy val pairElem: Parsley[PairElem]
        = FstPair("fst" ~> lvalue) | 
        SndPair("snd" ~> lvalue)

    lazy val arrayLiteral = ArrayLiteral("[" ~> sepBy(expr, ",") <~ "]")

    //private lazy val asgns = endBy(atomic(asgn), ";")
    //private lazy val asgn = Asgn(atomic(ident <~ "="), expr)


    ////////// EXPR PARSER ///////////

    lazy val atom
        = IntVal(intLiteral) | 
        BoolVal(boolLiteral) | 
        CharVal(charLiteral) | 
        StrVal(stringLiteral) | 
        (PairVal from pairLiteral) | 
        atomic(arrayElem) | 
        Var(ident) | 
        ("(" ~> expr <~ ")")

    lazy val arrayElem
        = atomic(ArrayVal(ident, some("[" ~> expr <~ "]")))

    // todo: fix problem with parsing len/ord/chr
    lazy val expr: Parsley[Expr] = 
        precedence(atom)(
            Ops(Prefix)(Not from "!", Neg from atomic("-" <~ notFollowedBy(digit)), Len from atomic("len"), Ord from atomic("ord"), Chr from atomic("chr")),
            Ops(InfixL)(Mul from "*", Mod from "%", Div from "/"),
            Ops(InfixL)(Add from "+", Sub from "-"),
            Ops(InfixN)(Grt from ">", GrtEql from ">=", Less from "<", LessEql from "<="),
            Ops(InfixN)(Eql from "==", NotEql from "!="),
            Ops(InfixR)(And from "&&"),
            Ops(InfixR)(Or from "||")
        )
}
