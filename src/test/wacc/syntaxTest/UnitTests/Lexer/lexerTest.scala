package wacc

import parsley.Parsley
import parsley.token.{Lexer, predicate}
import parsley.token.Lexer

import parsley.{Result, Success, Failure}

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._


class lexerIdentifierTest extends AnyFlatSpec {
    
    "An ident" should "start with and underscore or character" in {
        lexer.ident.parse("_") shouldBe Success("_")
        lexer.ident.parse("a") shouldBe Success("a")
        lexer.ident.parse("B") shouldBe Success("B")
    }

    it should "not start with a digit" in {
        lexer.ident.parse("1") shouldBe a [Failure[_]]
        lexer.ident.parse("9sx") shouldBe a [Failure[_]]
    }

    it should "be able to contain digits, characters or underscore after first character" in {
        lexer.ident.parse("a283") shouldBe Success("a283")
        lexer.ident.parse ("alpha") shouldBe Success("alpha")
        lexer.ident.parse("__42") shouldBe Success("__42")
        lexer.ident.parse("test_") shouldBe Success("test_")

    }
}