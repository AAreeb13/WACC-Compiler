package wacc

import org.scalactic.Bool
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._

class  SemanticErrorTest extends AnyFlatSpec  {
  def main(args: Array[String]): Unit = {
    // Create a SemanticErrorCollector instance
    val errorCollector = new SemanticErrorCollector("testFile", "")

    // Add some sample errors
    errorCollector.addError((5, 10), ScopeError("variable", "redec", 3))
    errorCollector.addError((8, 20), UndefFunc("foo"))
    errorCollector.addError((10, 5), RedefFunc("bar", 7))
    errorCollector.addError((12, 15), ArgSizeFunc("baz", 3, 2))

    // Get the collected errors and format them
    val formattedErrors = errorCollector.formatErrors

    // Print the formatted errors
    println(formattedErrors)
  }
}
