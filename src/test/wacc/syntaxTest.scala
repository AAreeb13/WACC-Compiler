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
        lexer.ident.parse("1").isFailure
        lexer.ident.parse("9sx").isFailure
    }
    "An ident" should "be able to contain digits, characters or underscore in between" in {
        lexer.ident.parse("a283") shouldBe Success("a283")
        lexer.ident.parse ("alpha") shouldBe Success("alpha")
        lexer.ident.parse("__42__") shouldBe Success("__42__")
    }

}