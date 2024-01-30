package wacc

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._

import parsley.Parsley
import parsley.token.{Lexer, predicate}
import parsley.token.Lexer
import parsley.token.descriptions._
import parsley.{Result, Success, Failure}

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

class parserExpressionTest extends AnyFlatSpec {
    "An expr" should "match an atom" in {
        parser.expr.parse("1") shouldBe Success(IntVal(1))
        parser.expr.parse("\'c\'") shouldBe Success(CharVal('c'))
        parser.expr.parse("\"Hello Mahdi Ahmed\"") shouldBe Success(StrVal("Hello Mahdi Ahmed"))
        parser.expr.parse("variable") shouldBe Success(Var("variable"))
        parser.expr.parse("arr[3]") shouldBe Success(ArrayVal("arr", IntVal(3) :: Nil))
        parser.expr.parse("((((\"hello\"))))") shouldBe Success(StrVal("hello"))
        parser.expr.parse("null") shouldBe Success(PairVal)
        parser.expr.parse("true") shouldBe Success(BoolVal(true))
        parser.expr.parse("false") shouldBe Success(BoolVal(false))
    }
    it should "match not, '!', operated expressions" in {
        parser.expr.parse("!true") shouldBe Success(Not(BoolVal(true)))
    }
    it should "match negation, '-', on an expression" in {
        parser.expr.parse("-4") shouldBe Success(Neg(IntVal(4)))
    }
    it should "match len on an expression" in {
        
    }
}