package wacc

import parsley.Parsley
import parsley.token.{Lexer, predicate}
import parsley.token.Lexer

import parsley.{Result, Success, Failure}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._

class parserPrecedenceTests extends AnyFlatSpec {
    "\'!\'' prefix" should "have tightest precedence" in {
        parser.expr.parse("!x||y") shouldBe Success(Or(Not(Var("x")), Var("y")))
        parser.expr.parse("!true&&false") shouldBe Success(And(Not(BoolVal(true)),BoolVal(false)))
    }
    "\'-\' prefix" should "have tightest precedence" in {
        parser.expr.parse("-4+3") shouldBe Success(Add(Neg(IntVal(4)), IntVal(3)))
    }
    "\'len\' prefix" should "have tightest precedence" in {
        parser.expr.parse("len(x)+y") shouldBe Success(Add(Len(Var("x")), Var("y")))
    }
    "\'ord\' prefix" should "have tightest precedence" in {
        parser.expr.parse("ord('c')+123") shouldBe Success(Add(Ord(CharVal('c')), IntVal(123)))
    }
    "\'chr\' prefix" should "have tightest precedence" in {
        parser.expr.parse("chr(65)!=\'A\'") shouldBe Success(NotEql(Chr(IntVal(65)), CharVal('A')))
    }

    "\'*\' Inflix Left" should "have second tightest precedence" in {
        parser.expr.parse("4*3+5") shouldBe Success(Add(Mul(IntVal(4), IntVal(3)), IntVal(5)))
        parser.expr.parse("4-3*5") shouldBe Success(Sub((IntVal(4)), Mul(IntVal(3), IntVal(5))))
    }
    "\'%\' Inflix Left" should "have second tightest precedence" in {
        parser.expr.parse("1%2*3") shouldBe Success(Mul(Mod(IntVal(1),IntVal(2)),IntVal(3)))
        parser.expr.parse("a%b+c") shouldBe Success(Add(Mod(Var("a"), Var("b")), Var("c")))
    }
    "\'/\' Inflix Left" should "have second tightest precedence" in {
        parser.expr.parse("4/3+5") shouldBe Success(Add(Div(IntVal(4), IntVal(3)), IntVal(5)))
        parser.expr.parse("4-c/5") shouldBe Success(Sub((IntVal(4)), Div(Var("c"), IntVal(5))))

    }


} 