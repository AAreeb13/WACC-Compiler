package wacc

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._

import parsley.Parsley
import parsley.token.{Lexer, predicate}
import parsley.token.Lexer
import parsley.token.descriptions._
import parsley.{Result, Success, Failure}
import org.scalactic.Bool

class parserStatementTest extends AnyFlatSpec {
    "\'skip\'" should "match stmt" in {
        parser.stmt.parse("skip") shouldBe Success(Skip)
    }
    "\'free\'" should "match stmt" in {
        parser.stmt.parse("free array") shouldBe Success(Free(Var("array")))
    }
    "New assignments" should "match stmt" in {
        parser.stmt.parse("int i=0") shouldBe Success(AssignNew(IntType, "i", IntVal(0)))
    }
    "assignments" should "match stmt" in {
        parser.stmt.parse("arr[1]=true") shouldBe Success(Assign(ArrayVal("arr", IntVal(1) :: Nil), BoolVal(true)))
    }
    "\'read\' command" should "match stmt" in {
        parser.stmt.parse("read x") shouldBe Success(Read(Var("x")))
    }
    "\'return\' with expr" should "match stmt" in {
        parser.stmt.parse("return(4+3)") shouldBe Success(Return(Add(IntVal(4), IntVal(3))))
    }
    "\'exit\' command" should "match stmt" in {
        parser.stmt.parse("exit(2000)") shouldBe Success(Exit(IntVal(2000)))
    }
    "\'print\'' statements" should "match stmt" in {
        parser.stmt.parse("print(\"Hello World\")") shouldBe Success(Print(StrVal("Hello World")))
    }
    "\'println\'' statements" should "match stmt" in {
        parser.stmt.parse("println(\"Hello Universe\")") shouldBe Success(Println(StrVal("Hello Universe")))
    }
    "\'if then else fi statements" should "match stmt" in {
        parser.stmt.parse("if(x==4)then return(5) else return(4) fi") shouldBe 
        Success(If(Eql(Var("x"), IntVal(4)), Return(IntVal(5)) :: Nil, Return(IntVal(4)) :: Nil))
    }

    "\'while do done\'' statements" should "match stmt" in {
        parser.stmt.parse("while(x==4) do arr[1]=true done") shouldBe 
        Success(While(Eql(Var("x"), IntVal(4)), Assign(ArrayVal("arr", IntVal(1) :: Nil), BoolVal(true)) :: Nil))
    }

    "\'begin end\'' statements" should "match stmt" in {
        parser.stmt.parse("begin int i=0 end") shouldBe Success(Scope(AssignNew(IntType, "i", IntVal(0)) :: Nil))
    }
}