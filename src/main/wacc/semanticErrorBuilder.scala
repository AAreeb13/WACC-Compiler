
// import parsley.errors.ErrorBuilder
// import scala.collection.mutable.ListBuffer
// import parsley.position

// case class SemanticError(position: (Int, Int), fileName: String, lines: Error, codeSnippet : Seq[String]) {
//     def formatFullError() : String = {
//         val formatStr = new StringBuilder
//         lines match {
//             case typeError: TypeError => formatStr.append("Type error")
//             case scopeError: ScopeError => formatStr.append("Scope error")
//             case undefFunc : UndefFunc => formatStr.append("Undefined function error")
//             case redefFunc : RedefFunc => formatStr.append("Function Scope Error")
//             case argSizeFunc : ArgSizeFunc => formatStr.append("Function call error")
//             case specialError: SpecialError => formatStr.append(specialError.errType)
//         }
//         formatStr.append(s" in ${fileName} ${position}")
//         formatStr.append(lines.formatError())
//         for (codeline: String <- codeSnippet) {
//             formatStr.append(s"${codeline}\n")
//         }
//         return formatStr.toString
//     }
// }


// sealed trait Error {
//     def formatError(): String
// }

// case class TypeError(unexpected: String, expected: String) extends Error {
//     override def formatError(): String = s"unexpected ${unexpected}\nexpected ${expected} \n"   
// }

// case class ScopeError(variable: String, errType: String, lineNum : Int) extends Error {
//     // errType = redec | undec
//     override def formatError(): String = {
//         errType match {
//             case "redec" => (s"illegal redeclaration of variable ${variable}" +
//               s"\npreviously declared on line ${lineNum}")
//             case "undec" => (s"variable ${variable} has not been declared in this scope\n")
//         }
//     }
// }


// sealed trait SpecialError extends Error {
//     val errType: String
// }

// trait FunctionError extends Error 
// case class UndefFunc(funName: String) extends FunctionError {

//   override def formatError(): String = s"${funName} has not been defined"

// }

// case class RedefFunc(funName: String, lineNum : Int) extends FunctionError {

//   override def formatError(): String = s"Illegal redefinition of function ${funName}\n" +
//   s"Previously declared on line ${lineNum}"

// }

// case class ArgSizeFunc(funName: String, unexpected : Int, expected : Int) extends FunctionError {

//   override def formatError(): String = s"Wrong number of arguments provided to function ${funName}\n" +
//   s"unexpected ${unexpected} arguments\nexpected ${expected} arguments"

// }




// case class PairExchange() extends SpecialError {

//   override def formatError(): String = ???

//   override val errType: String = ???

// }

// // sealed trait ErrorItem
// // case class SemanticRaw(item: String) extends ErrorItem
// // case class SemanticNamed(item: String) extends ErrorItem
// // case object SemanticEndOfInput extends  ErrorItem

// class SemanticErrorCollector(fileName : String, input : String) {
//     // Mutable list buffer to store semantic errors
//     private val semanticErrors: ListBuffer[SemanticError] = ListBuffer.empty
//     private val wholeCodeArray : Array[String] = input.split("(?<=\n)")
    
//     // Method to add a new semantic error
//     def addError(position: (Int, Int), lines: Error): Unit = {
//         semanticErrors += new SemanticError(position, fileName, lines, getCodeSnippet(position))
//     }
    
//     // Method to get the collected semantic errors
//     def getSemanticErrors: Seq[SemanticError] = semanticErrors.toList

//     def formatErrors : String = {
//         val builder = new StringBuilder
//         builder.append("Errors detected during compilation! Exit code 200 returned.")
//         for (errorLine <- getSemanticErrors) {
//             builder.append(errorLine.formatFullError() + "\n\n")
//         }
//         return builder.toString()
//     }

//     def getCodeSnippet (position:(Int, Int), linesAbove: Int = 2, linesBelow: Int = 2): Seq[String] = {
//       var codelines: ListBuffer[String] = ListBuffer.empty
//       for (lineNo <- position._1 - linesAbove to position._1 + linesBelow) {
//                   if (lineNo >= 1 && lineNo <= wholeCodeArray.length) {
//                 codelines.append(s"|${lineNo}     ${wholeCodeArray(lineNo - 1)}")
//             }
//         }
//         return codelines.toSeq

//     }
// }

// input hold
// class SemanticErrorBuilder(implicit val input: String, fileName: String) {
    

//     // THe whole code file line by line.
//     val wholeCodeArray: Array[String] = input.split("(?<=\n)")

//     def format(pos: (Int, Int), source: String, lines: ErrorInfoLines): SemanticError = {
//         val codeSnippet = getCodeSnippet(pos)
//         SemanticError(position = pos, fileName = fileName, lines = UndefinedFunc("Nothing"), codeSnippet = codeSnippet)
//     }

//     type Position = (Int, Int)
//     def pos(line: Int, col: Int): Position = (line, col)

//     type Source = String
    
//     type ErrorInfoLines = Error

//     type LineInfo = String

    
//     // def typeError(unexpected: String, expected: String, reasons: Messages, line: LineInfo): ErrorInfoLines = {
//     //     TypeError(unexpected, expected)
//     // }
//     // type UnexpectedLine = Option[Item]
//     // type Item = ErrorItem
//     // type ExpectedItems = Set[Item]
//     // type ExpectedLine = ExpectedItems
//     // type raw = SemanticRaw
//     // type Named = SemanticNamed

//     // type Message = String
//     // type Messages = Set[Message]

//     // def reason(reason: String): Message = reason
//     // def message(msg: String): Message = msg


// }


///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////



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


/*

-- Compiling...
Errors detected during compilation! Exit code 200 returned.
Function redefinition error in new.wacc (6, 3):
  illegal redefinition of function teri
  previously declared on line 2
  |  
  |  char teri(char x) is
  |  ^^^^
  |    return x

Scope error in new.wacc (9, 3):
  variable y has not been declared in this scope
  |  end
  |  y = 10;
  |  ^
  |  int y = 'a';

Type error in new.wacc (10, 11):
  unexpected char
  expected int
  |  y = 10;
  |  int y = 'a';
  |          ^
  |  int ab = 10;

Scope error in new.wacc (12, 7):
  illegal redeclaration of variable ab
  previously declared (in this scope) on line 11
  |  int ab = 10;
  |  int ab = 20;
  |      ^
  |  x = call func()

Scope error in new.wacc (13, 3):
  variable x has not been declared in this scope
  relevant in-scope variables include:
    int ab (declared on line 12)
    int y (declared on line 10)
  |  int ab = 20;
  |  x = call func()
  |  ^
  |end

Undefined function error in new.wacc (13, 7):
  function func has not been defined
  |  int ab = 20;
  |  x = call func()
  |      ^^^^
  |end
*/