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

class basetypeTest extends AnyFlatSpec {
    val bT = lexer.fully(parser.baseType)
    "Base type" should "do smt" in {
        bT.parse("") shouldBe true
    }

    val testT = lexer.fully(parser.stmt) 
    "You" should "say hi" in {
        testT.parse("int x = int") shouldBe true
    }
}