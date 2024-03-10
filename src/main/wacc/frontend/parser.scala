package wacc

import parsley.Parsley
import parsley.Result
import parsley.character._
import parsley.combinator._
import parsley.debug._
import parsley.errors.ErrorBuilder
import parsley.errors.combinator._
import parsley.expr.InfixL
import parsley.expr.InfixN
import parsley.expr.InfixR
import parsley.expr.Ops
import parsley.expr.Postfix
import parsley.expr.Prefix
import parsley.expr.chain
import parsley.expr.precedence
import parsley.syntax.zipped._
import java.io.File
import parsley.errors.patterns.VerifiedErrors

import Parsley._
import lexer._
import lexer.implicits.implicitSymbol
import scala.util
import ast._
import scala.util

/**
  * File which handles parsing input and generating an AST from it
  */

object parser {
    def parse(input: String): Either[String, Node] = parser.parse(input).toEither

    lazy val parser: Parsley[Prog] = fully(prog)

    // verified explains for additional clarity
    val _semicheck = "end".verifiedExplain("semi-colons may not appear at the end of a block")
    val _equalscheck = "=".label("assignment")
    val _opensqbrcheck = "[".label("array index") /* open square bracket check */

/*--------------------------------------- Type Parsers ---------------------------------------*/

    lazy val declType: Parsley[Type] = arrayType |
        pairType |
        baseType

    lazy val voidType = (VoidType.from("void"))
    lazy val retType: Parsley[TypeRef] = ((VoidType.from("void"))) | declType

    lazy val baseType: Parsley[BaseType] = ((IntType.from("int")) |
        (StringType.from("string")) |
        (CharType.from("char")) |
        (BoolType.from("bool")))

    lazy val arrayType: Parsley[Type]
        = atomic(chain.postfix1(baseType | pairType)(ArrayType.from("[" <~> "]")))

    lazy val pairType = PairType("pair" ~> "(" ~> pairElemType, "," ~> pairElemType <~ ")")

    lazy val pairElemType = arrayType |
        baseType |
        (ErasedPair.from("pair"))

 /*--------------------------------------- Statement Parsers ---------------------------------------*/

    lazy val prog = "begin".explain("A valid wacc program must start with begin") ~> Prog(funcList, stmtList) <~ "end".explain(
        "A valid wacc program must finish with end")
    lazy val funcList = many(func)

    lazy val voidFunc = Func(atomic(voidType <~> ident <~ "("), paramList <~ ")", "is" ~> stmtList <~ "end")
    lazy val nonVoidFunc = Func(atomic(declType <~> ident <~ "("), paramList <~ ")", "is" ~> stmtList.filter(containsReturn(_)) <~ "end")
    lazy val func = ifS(lookAhead(voidType).as(true) | lookAhead(declType).as(false), voidFunc, nonVoidFunc)

    lazy val paramList = sepBy(param, ",")
    lazy val param     = Param(declType, ident)
    lazy val stmtList: Parsley[List[Stat]] = sepBy1(stmt | _semicheck, ";")

    lazy val stmt = (Skip from "skip") |
        AssignNew(declType, ident <~ "=", rvalue) |
        (Assign(lvalue <~  _equalscheck, rvalue)) |
        Read("read" ~> lvalue) |
        Free("free" ~> expr).hide |
        Return("return" ~> option(expr)) |
        Exit("exit" ~> expr).hide |
        Print("print" ~> expr).hide |
        Println("println" ~> expr).hide |
        (If("if" ~> expr, "then" ~> stmtList, "else" ~> stmtList <~ "fi")) |
        (While("while" ~> expr, "do" ~> stmtList <~ "done")) |
        Scope("begin" ~> stmtList <~ "end") |
        CallStat("call" ~> ident, "(" ~> argList <~ ")")

    lazy val lvalue: Parsley[LValue] = VarOrArrayVal(ident, many(_opensqbrcheck ~> expr <~ "]")) |
        pairElem

    lazy val rvalue: Parsley[RValue] = expr |
        arrayLiteral |
        PairCons(atomic("newpair") ~> "(" ~> expr, "," ~> expr <~ ")") |
        ArrayCons("newarray" ~> declType, "[" ~> expr <~ "]") |
        pairElem |
        FuncCall("call" ~> ident, "(" ~> argList <~ ")")

    lazy val argList = sepBy(expr, ",")
    lazy val pairElem: Parsley[PairElem] = FstPair("fst" ~> lvalue) |
        SndPair("snd" ~> lvalue)

    lazy val arrayLiteral = ArrayLiteral("[".hide ~> sepBy(expr, ",") <~ "]")

/*--------------------------------------- Expression Parsers ---------------------------------------*/

    lazy val atom = IntVal(intLiteral) |
        BoolVal(boolLiteral) |
        CharVal(charLiteral) |
        StrVal(stringLiteral) |
        (PairVal.from(pairLiteral)) |
        VarOrArrayVal(ident, many("[" ~> expr <~ "]")) |
        ("(".hide ~> expr <~ ")")

    lazy val arrayElem = VarOrArrayVal(ident, many("[" ~> expr <~ "]"))

    // expression parsing using precedence
    lazy val expr: Parsley[Expr] =
        precedence(atom)(
            Ops(Prefix)(
                Not.from("!"),
                Neg.from(atomic("-" <~ notFollowedBy(digit))), // so that negative integer literals can be parsed properly
                Len.from("len"),
                Ord.from("ord"),
                Chr.from("chr"),
                BNeg.from("~")
            ),
            Ops(InfixL)(Mul.from("*"), Mod.from("%"), Div.from("/")),
            Ops(InfixL)(Add.from("+"), Sub.from("-")),
            Ops(InfixN)(Grt.from(">"), GrtEql.from(">="), Less.from("<"), LessEql.from("<=")),
            Ops(InfixN)(Eql.from("=="), NotEql.from("!=")),
            Ops(InfixL)(BAnd.from(atomic("&" <~ notFollowedBy("&")))),
            Ops(InfixL)(BOr.from(atomic("|" <~ notFollowedBy("|")))),
            Ops(InfixR)(And.from("&&")),
            Ops(InfixR)(Or.from("||"))
        )
}
