package wacc

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._

import parsley.Parsley
import parsley.token.{Lexer, predicate}
import parsley.token.Lexer
import parsley.token.descriptions._
import parsley.{Result, Success, Failure}

class lexerIdentifierTest extends AnyFlatSpec {
    
    "An ident" should "start with and underscore or character" in {
        lexer.ident.parse("_") shouldBe Success("_")
        lexer.ident.parse("a") shouldBe Success("a")
        lexer.ident.parse("B") shouldBe Success("B")
    }

    "An ident" should "not start with a digit" in {
        lexer.ident.parse("1").isFailure shouldBe true
        lexer.ident.parse("9sx").isFailure shouldBe true
    }
    "An ident" should "be able to contain digits, characters or underscore in between" in {
        lexer.ident.parse("a283") shouldBe Success("a283")
        lexer.ident.parse ("alpha") shouldBe Success("alpha")
        lexer.ident.parse("__42__") shouldBe Success("__42__")
    }
}

class parserExpressionTest extends AnyFlatSpec {
    "An expr" should "match an atom" in {
        parser.expr.parse("1") shouldBe Success(IntVal(1))
        parser.expr.parse("\'c\'") shouldBe Success(CharVal('c'))
        parser.expr.parse("\"Hello Mahdi Ahmed\"") shouldBe Success(StrVal("Hello Mahdi Ahmed"))
        parser.expr.parse("variable") shouldBe Success(Var("variable"))
        //parser.expr.parse("arr[3]") shouldBe Success(ArrayVal("arr", IntVal(3) :: Nil))
        parser.expr.parse("((((\"hello\"))))") shouldBe Success(StrVal("hello"))
        parser.expr.parse("arr[true]").isFailure shouldBe true
        parser.expr.parse("null") shouldBe Success(PairVal)
        parser.expr.parse("true") shouldBe Success(BoolVal(true))
        parser.expr.parse("false") shouldBe Success(BoolVal(false))
        parser.expr.parse("((((\"hello\"))))") shouldBe Success(StrVal("hello"))
    }
}