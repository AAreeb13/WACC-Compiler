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
        parser.stmt.parse("free(array)") shouldBe Success(Free(Var("array")))
    }
    
    
}