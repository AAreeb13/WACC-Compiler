
// import parsley.errors.ErrorBuilder
// import scala.collection.mutable.ListBuffer

// case class SemanticError(position: (Int, Int), source: String, lines: Error, code : Seq[String]) {
//     def formatFullError() : String = {
//         val formatStr = new StringBuilder
//         formatStr.append("Exit code 200 returned.\n")
//         lines match {
//             case typeError: TypeError => formatStr.append(s"Type error")
//             case scopeError: ScopeError => formatStr.append(s"Scope error")
//             case specialError: SpecialError => formatStr.append(specialError.errType)
//         }
//         formatStr.append(s" in ${source} ${position}")
//         formatStr.append(lines.formatError())
//         for (codeline: String <- code) {
//             formatStr.append(s"|    ${codeline} \n")
//         }
//         return formatStr.toString
//     }
// }


// sealed trait Error {
//     def formatError(): String
// }

// case class TypeError(unexpected: String, expected: String) extends Error {
//     override def formatError(): String = {
//         val formatStr: StringBuilder = new StringBuilder

//         formatStr.append(s"  unexpected ${unexpected} \n")
//         formatStr.append(s"  expected ${expected} \n")

//         for (item: ErrorItem <- expecteds) {
//             item match {
//                 case SemanticNamed(types) => formatStr.append(s"types \n")
//             }
//         }
        
//         return formatStr.toString()
//     }
// }

// case class ScopeError(variable: String, errType: String, lineNum : Int) extends Error {
//     // errType = redec | undec
//     override def formatError(): String = {
//         val formatStr: StringBuilder = new StringBuilder
//         errType match {
//             case "reDec" => formatStr.append(s"illegal redeclaration of variable ${variable}\npreviously declared on line ${lineNum}")
//             case "unDec" => formatStr.append(s"variable ${variable} has not been declared in this scope\n")
//         }
//         return formatStr.toString()
//     }
// }

// // Function Errors: Redefining function, Undefined function
// sealed trait SpecialError extends Error
// case class UndefinedFunc(funName: String) extends SpecialError
// case class RedefinedFunc(funName: String) extends SpecialError
// case class PairExchange

// sealed trait ErrorItem
// case class SemanticRaw(item: String) extends ErrorItem
// case class SemanticNamed(item: String) extends ErrorItem
// case object SemanticEndOfInput extends  ErrorItem

// class SemanticErrorCollector(builder: SemanticErrorBuilder, source: String ) {
//     // Mutable list buffer to store semantic errors
//     private val semanticErrors: ListBuffer[SemanticError] = ListBuffer.empty
    
//     // Method to add a new semantic error
//     def addError(position: (Int, Int), lines: Error): Unit = {
//         semanticErrors += builder.format(position, (), lines)
//     }
    
//     // Method to get the collected semantic errors
//     def getSemanticErrors: Seq[SemanticError] = semanticErrors.toList

//     def formatErrors : String = {
//         val builder = new StringBuilder
//         builder.append(s"Semantic Errors in %source\n")
//         for (errorLine <- getSemanticErrors) {
//             builder.append(errorLine.toString())
//         }
//     }
// }


// class SemanticErrorBuilder {
    
//     val fileName: Source

//     override def format(pos: (Int, Int), source: Unit, lines: ErrorInfoLines): SemanticError = SemanticError(pos, lines)

//     type Position = (Int, Int)
//     override def pos(line: Int, col: Int): Position = (line, col)

//     type Source = String
    
//     type ErrorInfoLines = Error

    
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







// // Define the semantic error case class and sealed traits
// case class SemanticError(position: (Int, Int), lines: Error)
// sealed trait Error
// case class TypeError(unexpected: Option[ErrorItem], expecteds: Set[ErrorItem], reasons: Set[String], width: Int) extends Error
// case class ScopeError(msgs: Set[String], width: Int) extends Error
// sealed trait ErrorItem
// case class SemanticRaw(item: String) extends ErrorItem
// case class SemanticNamed(item: String) extends ErrorItem
// case object SemanticEndOfInput extends ErrorItem

// // Define the SemanticErrorBuilder class implementing ErrorBuilder trait
// class SemanticErrorBuilder extends ErrorBuilder[SemanticError] {
    
//     // Implement the format method to create SemanticError instances
//     override def format(pos: (Int, Int), source: Unit, lines: ErrorInfoLines): SemanticError = SemanticError(pos, lines)

    
//     // Define the types needed for the builder
//     type Position = (Int, Int)

//     override def pos(line: Int, col: Int): Position = (line, col)
//     type Source = Unit
//     type ErrorInfoLines = Error
    
//     // Implement the typeError method to create TypeError instances
//     override def typeError(unexpected: UnexpectedLine, expected: ExpectedLine, reasons: Messages, line: LineInfo): ErrorInfoLines = {
//         TypeError(unexpected, expected, reasons, line)
//     }
    
//     // Define the types for error items and messages
//     type UnexpectedLine = Option[Item]
//     type Item = ErrorItem
//     type ExpectedItems = Set[Item]
//     type ExpectedLine = ExpectedItems
//     type Message = String
//     type Messages = Set[Message]
    
//     // Implement the reason and message methods
//     override def reason(reason: String): Message = reason
//     override def message(msg: String): Message = msg
// }


// // Example usage
// val errorBuilder = new SemanticErrorBuilder
// val errorCollector = new SemanticErrorCollector(errorBuilder)

// // Traverse the AST and add semantic errors as needed
// errorCollector.addError((1, 1), TypeError(None, Set.empty, Set("Invalid type"), 0))
// errorCollector.addError((2, 3), ScopeError(Set("Scope issue"), 0))

// // Get the collected semantic errors
// val collectedErrors = errorCollector.getSemanticErrors


// /*

// -- Compiling...
// Errors detected during compilation! Exit code 200 returned.
// Function redefinition error in new.wacc (6, 3):
//   illegal redefinition of function teri
//   previously declared on line 2
//   |  
//   |  char teri(char x) is
//   |  ^^^^
//   |    return x

// Scope error in new.wacc (9, 3):
//   variable y has not been declared in this scope
//   |  end
//   |  y = 10;
//   |  ^
//   |  int y = 'a';

// Type error in new.wacc (10, 11):
//   unexpected char
//   expected int
//   |  y = 10;
//   |  int y = 'a';
//   |          ^
//   |  int ab = 10;

// Scope error in new.wacc (12, 7):
//   illegal redeclaration of variable ab
//   previously declared (in this scope) on line 11
//   |  int ab = 10;
//   |  int ab = 20;
//   |      ^
//   |  x = call func()

// Scope error in new.wacc (13, 3):
//   variable x has not been declared in this scope
//   relevant in-scope variables include:
//     int ab (declared on line 12)
//     int y (declared on line 10)
//   |  int ab = 20;
//   |  x = call func()
//   |  ^
//   |end

// Undefined function error in new.wacc (13, 7):
//   function func has not been defined
//   |  int ab = 20;
//   |  x = call func()
//   |      ^^^^
//   |end
// */