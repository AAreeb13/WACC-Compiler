package wacc.unit.syntax.parser

import wacc._
import org.scalactic.Bool
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._
import parsley.Failure
import parsley.Parsley
import parsley.Result
import parsley.Success
import parsley.token.Lexer
import parsley.token.descriptions._
import parsley.token.predicate

class parserExpressionTest extends AnyFlatSpec {
    val exprParser = lexer.fully(parser.expr)

    "An expr" should "match an atom" in {
        exprParser.parse("1") shouldBe Success(IntVal(1))
        exprParser.parse("\'c\'") shouldBe Success(CharVal('c'))
        exprParser.parse("\"Hello Mahdi Ahmed\"") shouldBe Success(StrVal("Hello Mahdi Ahmed"))
        exprParser.parse("variable") shouldBe Success(Var("variable"))
        exprParser.parse("arr[3]") shouldBe Success(ArrayVal("arr", List(IntVal(3))))
        exprParser.parse("((((\"hello\"))))") shouldBe Success(StrVal("hello"))
        exprParser.parse("null") shouldBe Success(PairVal)
        exprParser.parse("true") shouldBe Success(BoolVal(true))
        exprParser.parse("false") shouldBe Success(BoolVal(false))
    }

    it should "match not, '!', operated expr" in {
        exprParser.parse("!true") shouldBe Success(Not(BoolVal(true)))
    }

    it should "match negation, '-', on an expr, except in the case of an int literal" in {
        exprParser.parse("-x") shouldBe Success(Neg(Var("x")))
        exprParser.parse("-4") shouldBe Success(IntVal(-4))
        exprParser.parse("-4") should not be Success(Neg(IntVal(4)))
    }

    it should "match len on an expr" in {
        exprParser.parse("len(arr)") shouldBe Success(Len(Var("arr")))
        exprParser.parse("len(arr[1])") shouldBe Success(Len(ArrayVal("arr", List(IntVal(1)))))
    }

    it should "match ord on an expr" in {
        exprParser.parse("ord(x)") shouldBe Success(Ord(Var("x")))
        exprParser.parse("ord(\'c\')") shouldBe Success(Ord(CharVal('c')))
    }

    it should "match binary logical and/or symbols" in {
        exprParser.parse("true&&false") shouldBe Success(And(BoolVal(true), BoolVal(false)))
        exprParser.parse("x||true") shouldBe Success(Or(Var("x"), BoolVal(true)))
    }

    it should "match with equality and inequality symbols between two exprs" in {
        exprParser.parse("x==6") shouldBe Success(Eql(Var("x"), IntVal(6)))
        exprParser.parse("y!=x") shouldBe Success(NotEql(Var("y"), Var("x")))
        exprParser.parse("x<5") shouldBe Success(Less(Var("x"), IntVal(5)))
        exprParser.parse("x>5") shouldBe Success(Grt(Var("x"), IntVal(5)))
        exprParser.parse("x<=5") shouldBe Success(LessEql(Var("x"), IntVal(5)))
        exprParser.parse("x>=5") shouldBe Success(GrtEql(Var("x"), IntVal(5)))
    }

    it should "match with binary operators between two expressions" in {
        exprParser.parse("x*4") shouldBe Success(Mul(Var("x"), IntVal(4)))
        exprParser.parse("5+a") shouldBe Success(Add(IntVal(5), Var("a")))
        exprParser.parse("5-23") shouldBe Success(Sub(IntVal(5), IntVal(23)))
        exprParser.parse("23/x") shouldBe Success(Div(IntVal(23), Var("x")))
        exprParser.parse("y%x") shouldBe Success(Mod(Var("y"), Var("x")))
    }

    it should "match binary logical and/or symbols with spaces" in {
        exprParser.parse("true  &&  false") shouldBe Success(And(BoolVal(true), BoolVal(false)))
        exprParser.parse("x  ||  true") shouldBe Success(Or(Var("x"), BoolVal(true)))
    }

    it should "match unary operations with spaces" in {
        exprParser.parse("len arr") shouldBe Success(Len(Var("arr")))
        exprParser.parse("ord x") shouldBe Success(Ord(Var("x")))
    }

    it should "match with equality and inequality symbols with unnecessary spaces" in {
        exprParser.parse("y != x") shouldBe Success(NotEql(Var("y"), Var("x")))
    }
}
