
// import parsley.errors.ErrorBuilder

// case class SemanticError(position: (Int, Int), lines: ErrorLines)

// sealed trait ErrorLines
// case class VanillaError(unexpected: Option[ErrorItem], expecteds: Set[ErrorItem], reasons: Set[String], width: Int) extends ErrorLines
// case class SpecialisedError(msgs: Set[String], width: Int) extends ErrorLines

// sealed trait ErrorItem
// case class SemanticRaw(item: String) extends ErrorItem
// case class SemanticNamed(item: String) extends ErrorItem
// case object SemanticEndOfInput extends  ErrorItem


// abstract class SemanticErrorBuilder extends ErrorBuilder[SemanticError] {
//     override def format(pos: (Int, Int), source: Unit, lines: ErrorInfoLines): SemanticError = SemanticError(pos, lines)
//     type Position = (Int, Int)
//     override def pos(line: Int, col: Int): Position = (line, col)
//     type Source = Unit
//     type ErrorInfoLines = ErrorLines

    
//     override def vanillaError(unexpected: UnexpectedLine, expected: ExpectedLine, reasons: Messages, line: LineInfo): ErrorInfoLines = {
//         VanillaError(unexpected, expected, reasons, line)
//     }
//     type UnexpectedLine = Option[Item]
//     type Item = ErrorItem
//     type ExpectedItems = Set[Item]
//     type ExpectedLine = ExpectedItems

//     type Message = String
//      type Messages = Set[Message]
//     override def reason(reason: String): Message = reason
//     override def message(msg: String): Message = msg
// }