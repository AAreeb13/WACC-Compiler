package wacc

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._

import parsley.Parsley
import parsley.token.{Lexer, predicate}
import parsley.token.Lexer
import parsley.token.descriptions._
import parsley.{Result, Success, Failure}
import org.scalactic.Bool

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
    it should "match not, '!', operated expr" in {
        parser.expr.parse("!true") shouldBe Success(Not(BoolVal(true)))
    }
    it should "match negation, '-', on an expr" in {
        parser.expr.parse("-4") shouldBe Success(Neg(IntVal(4)))
    }
    it should "match len on an expr" in {
        parser.expr.parse("len(arr)") shouldBe Success(Len(Var("arr")))
        parser.expr.parse("len(arr[1])") shouldBe Success(Len(ArrayVal("arr",IntVal(1) :: Nil)))                  
    }
    it should "match ord on an expr" in {
        parser.expr.parse("ord(x)") shouldBe Success(Ord(Var("x")))
        parser.expr.parse("ord(\'c\')") shouldBe Success(Ord(CharVal('c')))
    }
    it should "match binary logical and/or symbols" in {
        parser.expr.parse("true&&false") shouldBe Success(And(BoolVal(true), BoolVal(false)))
        parser.expr.parse("x||true") shouldBe Success(Or(Var("x"), BoolVal(true)))
    }
    
    it should "match with equality and inequality symbols between two exprs" in {
        parser.expr.parse("x==6") shouldBe Success(Eql(Var("x"), IntVal(6)))
        parser.expr.parse("y!=x") shouldBe Success(NotEql(Var("y"), Var("x")))
        parser.expr.parse("x<5") shouldBe Success(Less(Var("x"),IntVal(5)))
        parser.expr.parse("x>5") shouldBe Success(Grt(Var("x"),IntVal(5)))
        parser.expr.parse("x<=5") shouldBe Success(LessEql(Var("x"), IntVal(5)))
        parser.expr.parse("x>=5") shouldBe Success(GrtEql(Var("x"), IntVal(5)))
        
    }
    it should "match with binary operators between two expressions" in {
        parser.expr.parse("x*4") shouldBe Success(Mul(Var("x"), IntVal(4)))
        parser.expr.parse("5+a") shouldBe Success(Add(IntVal(5), Var("a")))
        parser.expr.parse("5-23") shouldBe Success(Sub(IntVal(5), IntVal(23)))
        parser.expr.parse("23/x") shouldBe Success(Div(IntVal(23), Var("x")))
        parser.expr.parse("y%x") shouldBe Success(Mod(Var("y"),Var("x")))
    }
    it should "match binary logical and/or symbols with spaces" in {
        parser.expr.parse("true  &&  false") shouldBe Success(And(BoolVal(true), BoolVal(false)))
        parser.expr.parse("x  ||  true") shouldBe Success(Or(Var("x"), BoolVal(true)))
    }
    it should "match unary operations with spaces" in {
        parser.expr.parse("len arr") shouldBe Success(Len(Var("arr")))
        parser.expr.parse("ord x") shouldBe Success(Ord(Var("x")))
    }
    it should "match with equality and inequality symbols with unnecessary spaces" in {
        parser.expr.parse("y != x") shouldBe Success(NotEql(Var("y"), Var("x")))
    }
}
