package wacc

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

class parserAtomTest extends AnyFlatSpec {
    val atomParser = lexer.fully(parser.atom)
    val funcParser = lexer.fully(parser.func)

    "An atom" should "match an integer value" in {
        atomParser.parse("1") shouldBe Success(IntVal(1))
    }

    it should "match a character" in {
        atomParser.parse("\'c\'") shouldBe Success(CharVal('c'))
    }

    it should "match a string" in {
        atomParser.parse("\"Hello Mahdi Ahmed\"") shouldBe Success(StrVal("Hello Mahdi Ahmed"))
    }

    it should "match a varable name" in {
        atomParser.parse("varName") shouldBe Success(Var("varName"))
    }

    it should "match an expression in nested in brackets" in {
        atomParser.parse("((((\"hello\"))))") shouldBe Success(StrVal("hello"))
    }

    it should "match a pair literal" in {
        atomParser.parse("null") shouldBe Success(PairVal)
    }

    it should "match a boolean" in {
        atomParser.parse("true") shouldBe Success(BoolVal(true))
        atomParser.parse("false") shouldBe Success(BoolVal(false))
    }

    it should "match an array element" in {
        atomParser.parse("arr[3]") shouldBe Success(ArrayVal("arr", List(IntVal(3))))
    }
}
