package wacc

import org.scalactic.Bool
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._

class  SemanticErrorTest extends AnyFlatSpec  {
    val errorCollector = new SemanticErrorCollector("testFile.wacc", "")

    // Add some sample errors
    // Get the collected errors and format them


    "ScopeError class with redeclared variable" should " display correct error type, variable name, and error location in a message" in {
       ScopeError("variable", "redec", 3).formatError() shouldBe "illegal redeclaration of variable \"variable\"\npreviously declared on line 3"
    }
    "ScopeError class with undeclared variable" should " display correct error type, variable name, and error location in a message" in {
       ScopeError("variable", "undec", 3).formatError() shouldBe "variable \"variable\" has not been declared in this scope\n"
    }

}
