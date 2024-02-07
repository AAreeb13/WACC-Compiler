package wacc.integration

import org.scalatest.flatspec.AnyFlatSpec
import wacc._
class IntegrationTest extends AnyFlatSpec {
    val exitSuccess     = 0
    val exitSyntaxErr   = 100
    val exitSemanticErr = 200
    val syntaxCheckOnly = true

    "Valid examples" should "compile successfully" in {
        runTests("wacc_examples/valid", exitSuccess) 
    }

    "Syntax error examples" should "produce a syntax error" in {
        runTests("wacc_examples/invalid/syntaxErr", exitSyntaxErr) 
    }

    "Semantic error examples" should "produce a semantic error" in {
        runTests("wacc_examples/invalid/semanticErr", exitSemanticErr) 
    }

    def runTests(path: String, expected: Int): Unit = {
    }
}