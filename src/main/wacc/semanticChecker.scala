package wacc

object semanticChecker {
    def verify(result: Either[String, Node]): Either[String, Node] = result.flatMap(_ match {
        case prog: Prog =>
            // Semantic checks will go here
            return result
        case _ => Left("Invalid AST type for top-level verification")
    })
}
