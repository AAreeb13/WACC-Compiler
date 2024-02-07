package wacc

import parsley.Parsley
import parsley.Result
import parsley.character.digit
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

import Parsley._
import lexer._
import lexer.implicits.implicitSymbol

object parser {
    // the Err: ErrorBuilder here is saying that the compiler must be able to find a value
    // of type ErrorBuilder[Err] to provide implicitly to this function. When you use this
    // in the REPL, it will default to ErrorBuilder[String]. In part 3, the tests will instantiate
    // it differently
    // If you like, you can think of it as having type:
    // def parse(input: String): Either[String, Prog]
    def parse[Err: ErrorBuilder](input: String): Result[Err, Node] = parser.parse(input)
    // def parse[Err: ErrorBuilder](input: String) = parser.parse(input)

    lazy val parser: Parsley[Node] = fully(prog)

    ////////// TYPE PARSER ///////////
    lazy val declType: Parsley[Type] = (arrayType |
        pairType |
        baseType).label("declarable Type").explain("declarable types are: arrayType, pairType and baseType")

    lazy val baseType: Parsley[BaseType] = ((IntType.from("int")) |
        (StringType.from("string")) |
        (CharType.from("char")) |
        (BoolType.from("bool"))).label("base type").explain("base types are : int, string, char, bool")

    lazy val arrayType: Parsley[Type]
    // = precedence(baseType, pairType)(Ops(Postfix)(ArrayType from "[" <~> "]"))
    = atomic(chain.postfix1(baseType | pairType)(ArrayType.from("[" <~> "]")))

    lazy val pairType = PairType(atomic("pair" ~> "(") ~> pairElemType, "," ~> pairElemType <~ ")")

    lazy val pairElemType = arrayType |
        baseType |
        (ErasedPair.from("pair"))

    // probably don't need the lastOption since grammar guarantees that
    // all filtered stmtLists are valid and contains at least one valid during filter
    // but just to be on the safe side
    private def containsReturn(stmtOption: Option[Stat]): Boolean = stmtOption match {
        case None => false
        case Some(stmt) =>
            stmt match {
                case Return(expr) => true
                case Exit(expr)   => true
                case If(cond, ifstmts, elsestmts) =>
                    containsReturn(ifstmts.lastOption) && containsReturn(elsestmts.lastOption)
                case While(cond, stmts) => containsReturn(stmts.lastOption)
                case Scope(stmts)       => containsReturn(stmts.lastOption)
                case _                  => false
            }
    }

    ////////// STAT PARSER ///////////

    lazy val prog     = "begin".explain(
        "WACC programs must start with begin") ~> Prog(funcList, stmtList) <~ "end".explain(
        "WACC programs must finish with end")
    lazy val funcList = many(func)
    lazy val func = atomic(
      Func(
        declType,
        ident,
        "(" ~> paramList <~ ")",
        "is" ~> stmtList.filter(stmts => containsReturn(stmts.lastOption)) <~ "end"
      )
    )

    lazy val paramList = sepBy(param, ",")
    lazy val param     = Param(declType, ident)

    lazy val stmtList: Parsley[List[Stat]] = sepBy1(stmt, ";")

    lazy val stmt = "skip".as(Skip) |
        (AssignNew(declType, ident <~ "=", rvalue)).label("variable declaration") |
        (Assign(atomic(lvalue <~ "="), rvalue)).label("assignment") |
        Read("read" ~> lvalue).hide |
        Free("free" ~> expr).hide |
        Return("return" ~> expr).label("return statement") |
        Exit("exit" ~> expr).hide |
        Print("print" ~> expr).hide |
        Println("println" ~> expr).hide |
        (If("if" ~> expr, "then" ~> stmtList, "else" ~> stmtList <~ "fi")).label("if statement") |
        (While("while" ~> expr, "do" ~> stmtList <~ "done")).label("while loop") |
        Scope("begin" ~> stmtList <~ "end").hide

    lazy val lvalue: Parsley[LValue] = arrayElem |
        Var(ident) |
        pairElem

    lazy val rvalue: Parsley[RValue] = expr |
        arrayLiteral |
        PairCons("newpair" ~> "(" ~> expr, "," ~> expr <~ ")") |
        pairElem |
        FuncCall("call" ~> ident, "(" ~> argList <~ ")")

    lazy val argList = sepBy(expr, ",")
    lazy val pairElem: Parsley[PairElem] = FstPair("fst" ~> lvalue) |
        SndPair("snd" ~> lvalue)

    lazy val arrayLiteral = ArrayLiteral("[" ~> sepBy(expr, ",") <~ "]")

    // private lazy val asgns = endBy(atomic(asgn), ";")
    // private lazy val asgn = Asgn(atomic(ident <~ "="), expr)

    ////////// EXPR PARSER ///////////

    lazy val atom = IntVal(intLiteral) |
        BoolVal(boolLiteral) |
        CharVal(charLiteral) |
        StrVal(stringLiteral) |
        (PairVal.from(pairLiteral)) |
        atomic(arrayElem) |
        Var(ident) |
        ("(" ~> expr <~ ")")

    lazy val arrayElem = atomic(ArrayVal(ident, some("[" ~> expr <~ "]")))

    // todo: fix problem with parsing len/ord/chr
    lazy val expr: Parsley[Expr] =
        precedence(atom)(
            Ops(Prefix)(
                Not.from("!"),
                Neg.from(atomic("-" <~ notFollowedBy(digit))),
                Len.from(atomic("len")),
                Ord.from(atomic("ord")),
                Chr.from(atomic("chr"))
            ),
            Ops(InfixL)(Mul.from("*"), Mod.from("%"), Div.from("/")),
            Ops(InfixL)(Add.from("+"), Sub.from("-")),
            Ops(InfixN)(Grt.from(">"), GrtEql.from(">="), Less.from("<"), LessEql.from("<=")),
            Ops(InfixN)(Eql.from("=="), NotEql.from("!=")),
            Ops(InfixR)(And.from("&&")),
            Ops(InfixR)(Or.from("||"))
        )
}
