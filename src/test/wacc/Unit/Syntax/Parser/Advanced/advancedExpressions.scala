package wacc.unit.syntax.parser.advanced

import wacc._
import astFactory._
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

class advancedExpressions extends AnyFlatSpec {
    "multiple \'+\' and \'-\' symbols" should "be nested with left association" in {
        parser.expr.parse("x+2-4+5+y") shouldBe Success(
          Add(Add(Sub(Add(Var("x"), IntVal(2)), IntVal(4)), IntVal(5)), Var("y"))
        )
    }
    "multiple \'*\', \'%\' and \'/\' symbols" should "be nested with left association" in {
        parser.expr.parse("x*2%4/5/y") shouldBe Success(
          Div(Div(Mod(Mul(Var("x"), IntVal(2)), IntVal(4)), IntVal(5)), Var("y"))
        )
    }
    "multiple \'>\', \'>=\', \'<\', \'<=\'" should "return an error" in {
        parser.expr.parse("x<y<z").isFailure shouldBe true
    }

}
