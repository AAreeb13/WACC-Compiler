package wacc

import parsley.Parsley
import parsley.token.{Lexer, predicate}
import parsley.token.Lexer
import parsley.{Result, Success, Failure}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._

class parserPrecedenceTests extends AnyFlatSpec {
    "\'!\'' prefix" should "have highest precedence" in {
        parser.expr.parse("!x||y") shouldBe Success(Or(Not(Var("x")), Var("y")))
        parser.expr.parse("!true&&false") shouldBe Success(And(Not(BoolVal(true)),BoolVal(false)))
    }
    "\'-\' prefix" should "have highest precedence" in {
        parser.expr.parse("-4+3") shouldBe Success(Add(Neg(IntVal(4)), IntVal(3)))
    }
} 