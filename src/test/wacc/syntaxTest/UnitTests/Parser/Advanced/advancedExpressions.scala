package wacc

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._

import parsley.Parsley
import parsley.token.{Lexer, predicate}
import parsley.token.Lexer
import parsley.token.descriptions._
import parsley.{Result, Success, Failure}
import org.scalactic.Bool

class advancedExpressions extends AnyFlatSpec {
    "multiple \'+\' and \'-\' symbols" should "be nested with left association" in {
        parser.expr.parse("x+2-4+5+y") shouldBe Success(Add(Add(Sub(Add(Var("x"), IntVal(2)), IntVal(4)), IntVal(5)), Var("y")))
    }
    "multiple \'*\', \'%\' and \'/\' symbols" should "be nested with left association" in {
        parser.expr.parse("x*2%4/5/y") shouldBe Success(Div(Div(Mod(Mul(Var("x"), IntVal(2)), IntVal(4)), IntVal(5)), Var("y")))
    }
    "multiple \'>\', \'>=\', \'<\', \'<=\'" should "return an error" in {
        parser.expr.parse("x<y<z").isFailure shouldBe true
    }

}