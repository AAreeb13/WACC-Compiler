package wacc

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._

import parsley.Parsley
import parsley.token.{Lexer, predicate}
import parsley.token.Lexer
import parsley.token.descriptions._
import parsley.{Result, Success, Failure}
import org.scalactic.Bool

class parserAtomTest extends AnyFlatSpec {
    "An atom" should "match an integer value" in {
        parser.atom.parse("1") shouldBe Success(IntVal(1))
    }
    it should "match a character" in {
        parser.atom.parse("\'c\'") shouldBe Success(CharVal('c'))
    }
    it should "match a string" in {
        parser.atom.parse("\"Hello Mahdi Ahmed\"") shouldBe Success(StrVal("Hello Mahdi Ahmed"))

    }
    it should "match a varable name" in {
        parser.atom.parse("varName") shouldBe Success(Var("varName"))
    }
    it should "match an expression in nested in brackets" in {
        parser.atom.parse("((((\"hello\"))))") shouldBe Success(StrVal("hello"))
    }
    it should "match with a pair literal" in {
        parser.atom.parse("null") shouldBe Success(PairVal)
    }
    it should "match with a boolean" in {
        parser.atom.parse("true") shouldBe Success(BoolVal(true))
        parser.atom.parse("false") shouldBe Success(BoolVal(false))        
    }
    it should "match with an array element" in {
        parser.expr.parse("arr[3]") shouldBe Success(ArrayVal("arr", IntVal(3) :: Nil))
    }    
}

