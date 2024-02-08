
// import parsley.errors.ErrorBuilder
// import scala.collection.mutable.ListBuffer

// case class SemanticError(position: (Int, Int), lines: ErrorLines)

// sealed trait ErrorLines
// case class TypeError(unexpected: Option[ErrorItem], expecteds: Set[ErrorItem], reasons: Set[String], width: Int) extends ErrorLines
// case class ScopeError(msgs: Set[String], width: Int) extends ErrorLines

// sealed trait ErrorItem
// case class SemanticRaw(item: String) extends ErrorItem
// case class SemanticNamed(item: String) extends ErrorItem
// case object SemanticEndOfInput extends  ErrorItem

// class SemanticErrorCollector(builder: SemanticErrorBuilder) {
//     // Mutable list buffer to store semantic errors
//     private val semanticErrors: ListBuffer[SemanticError] = ListBuffer.empty
    
//     // Method to add a new semantic error
//     def addError(position: (Int, Int), lines: ErrorLines): Unit = {
//         semanticErrors += builder.format(position, (), lines)
//     }
    
//     // Method to get the collected semantic errors
//     def getSemanticErrors: Seq[SemanticError] = semanticErrors.toList
// }


// class SemanticErrorBuilder extends ErrorBuilder[SemanticError] {
    
//     val fileName: Source

//     override def format(pos: (Int, Int), source: Unit, lines: ErrorInfoLines): SemanticError = SemanticError(pos, lines)

//     type Position = (Int, Int)
//     override def pos(line: Int, col: Int): Position = (line, col)

//     type Source = String
    
//     type ErrorInfoLines = ErrorLines

    
//     override def typeError(unexpected: UnexpectedLine, expected: ExpectedLine, reasons: Messages, line: LineInfo): ErrorInfoLines = {
//         TypeError(unexpected, expected, reasons, line)
//     }
//     type UnexpectedLine = Option[Item]
//     type Item = ErrorItem
//     type ExpectedItems = Set[Item]
//     type ExpectedLine = ExpectedItems
//     type raw = SemanticRaw
//     type Named = SemanticNamed

//     type Message = String
//     type Messages = Set[Message]

//     override def reason(reason: String): Message = reason
//     override def message(msg: String): Message = msg
// }




// // // Define the semantic error case class and sealed traits
// // case class SemanticError(position: (Int, Int), lines: ErrorLines)
// // sealed trait ErrorLines
// // case class TypeError(unexpected: Option[ErrorItem], expecteds: Set[ErrorItem], reasons: Set[String], width: Int) extends ErrorLines
// // case class ScopeError(msgs: Set[String], width: Int) extends ErrorLines
// // sealed trait ErrorItem
// // case class SemanticRaw(item: String) extends ErrorItem
// // case class SemanticNamed(item: String) extends ErrorItem
// // case object SemanticEndOfInput extends ErrorItem

// // // Define the SemanticErrorBuilder class implementing ErrorBuilder trait
// // class SemanticErrorBuilder extends ErrorBuilder[SemanticError] {
    
// //     // Implement the format method to create SemanticError instances
// //     override def format(pos: (Int, Int), source: Unit, lines: ErrorInfoLines): SemanticError = SemanticError(pos, lines)

    
// //     // Define the types needed for the builder
// //     type Position = (Int, Int)

// //     override def pos(line: Int, col: Int): Position = (line, col)
// //     type Source = Unit
// //     type ErrorInfoLines = ErrorLines
    
// //     // Implement the typeError method to create TypeError instances
// //     override def typeError(unexpected: UnexpectedLine, expected: ExpectedLine, reasons: Messages, line: LineInfo): ErrorInfoLines = {
// //         TypeError(unexpected, expected, reasons, line)
// //     }
    
// //     // Define the types for error items and messages
// //     type UnexpectedLine = Option[Item]
// //     type Item = ErrorItem
// //     type ExpectedItems = Set[Item]
// //     type ExpectedLine = ExpectedItems
// //     type Message = String
// //     type Messages = Set[Message]
    
// //     // Implement the reason and message methods
// //     override def reason(reason: String): Message = reason
// //     override def message(msg: String): Message = msg
// // }


// // Example usage
// val errorBuilder = new SemanticErrorBuilder
// val errorCollector = new SemanticErrorCollector(errorBuilder)

// // Traverse the AST and add semantic errors as needed
// errorCollector.addError((1, 1), TypeError(None, Set.empty, Set("Invalid type"), 0))
// errorCollector.addError((2, 3), ScopeError(Set("Scope issue"), 0))

// // Get the collected semantic errors
// val collectedErrors = errorCollector.getSemanticErrors
