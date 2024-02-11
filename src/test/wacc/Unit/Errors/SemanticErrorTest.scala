// package wacc.unit.errors

// import wacc._
// import org.scalactic.Bool
// import org.scalatest.flatspec.AnyFlatSpec
// import org.scalatest.matchers.should.Matchers._

// class  SemanticErrorTest extends AnyFlatSpec  {
//     val errorCollector = new SemanticErrorCollector("testFile.wacc", "")

//     // Add some sample errors
//     // Get the collected errors and format them

//     // Error Tests
//     "ScopeError class with redeclared variable" should " display correct error type, variable name, and error location in a message" in {
//       ScopeError("variable", "redec", 3).formatError() shouldBe "illegal redeclaration of variable \"variable\"\npreviously declared on line 3"
//     }
//     "ScopeError class with undeclared variable" should " display correct error type, variable name, and error location in a message" in {
//       ScopeError("variable", "undec", 3).formatError() shouldBe "variable \"variable\" has not been declared in this scope\n"
//     }

//     "TypeError class" should "display correct error message for unexpected and expected types" in {
//       TypeError("int", "bool").formatError() shouldBe "unexpected int\nexpected bool \n"
//     }

//     "UndefFunc class" should "display correct error message for an undefined function" in {
//       UndefFunc("foo").formatError() shouldBe "foo has not been defined"
//     }

//     "RedefFunc class" should "display correct error message for a redefined function" in {
//       RedefFunc("foo", 5).formatError() shouldBe "Illegal redefinition of function foo\nPreviously declared on line 5"
//     }

//     "ArgSizeFunc class" should "display correct error message for incorrect number of arguments" in {
//       ArgSizeFunc("foo", 3, 2).formatError() shouldBe "Wrong number of arguments provided to function foo\nunexpected 3 arguments\nexpected 2 arguments"
//     }

//     "SpecialError class" should "display the provided error message" in {
//       SpecialError("CustomError", "This is a custom error message").formatError() shouldBe "This is a custom error message"
//     }



//     Semantic Error Test
//     SemanticError(position: (Int, Int), fileName: String, lines: Error, codeSnippet : Seq[String], msg : String = "")
//     "SemanticError class" should "display the provided error message" in {
//       val error = ScopeError("variable", "redec", 3)
//       SemanticError((1,2), "wacky.wacc", error, Seq("|  int x = 1", "|  int y = 2")).formatFullError() shouldBe "Scope Error in wacky.wacc (1,2)\n" +
//       "illegal redeclaration of variable \"variable\"" +
//       "previously declared on line 3\n" +
//       "|  int x = 1\n" +
//       "|  int y = 2"
//     }
    
// }
