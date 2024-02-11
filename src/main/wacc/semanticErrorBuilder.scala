package wacc
import ast._
import parsley.errors.ErrorBuilder
import scala.collection.mutable.ListBuffer
import parsley.position

/*SemanticError abstracts out the components which are same across all errors,
  namely the line "X error in file.wacc position"
  and the code snippet
 */
case class SemanticError(node: Node, fileName: Option[String], errorType: Error, codeSnippet: Seq[String], msg: String = "") {
    def formatFullError: String = {
        val formatStr = new StringBuilder
        formatStr.append(errorType.toString())
        if (fileName.isDefined)
            formatStr.append(s" in ${fileName.get} ${node.pos}\n")
        formatStr.append(errorType.formatError + "\n")
        formatStr.append(msg + "\n")
        for (codeline: String <- codeSnippet) {
            formatStr.append(s"${codeline}\n")
        }
        return formatStr.toString
    }
}

sealed trait Error {
    def formatError: String
}

case class TypeError(unexpected: String, expected: String) extends Error {
    override def formatError = s"unexpected ${unexpected}\nexpected ${expected} \n"

    override def toString = "Type Error"
}

sealed trait ScopeError extends Error {
    override def toString = "Scope Error"
}

case class RedeclaredVarError(node: Node, prevNode: Option[Node]) extends ScopeError {
    override def formatError = {
        val sb = new StringBuilder
        val ident = node match {
            case Var(name) => name
            case ArrayVal(name, _) => name
            case Param(_, name) => name
            case _ => "unknown"
        }
        sb.append(s"illegal redeclaration of variable \"${ident}\"")
        if (prevNode.isDefined) {
            sb.append(s"\npreviously declared on line ${prevNode.get.pos._1}")
        }
        sb.toString
    }
}

case class UndeclaredVarError(node: Node) extends ScopeError {
    override def formatError = {
        val ident = node match {
            case Var(name) => name
            case ArrayVal(name, _) => name
            case _ => "unknown"
        }
        s"variable \"${ident}\" has not been declared in this scope\n"
    }
}

trait FunctionError extends Error
case class UndefinedFuncError(funcName: String) extends FunctionError {

    override def formatError = s"${funcName} has not been defined"

    override def toString = "Undefined Function Error"

}

case class RedefinedFuncError(func: Func, prevFunc: Option[Func]) extends FunctionError {

    override def formatError = {
        val sb = new StringBuilder
        sb ++= s"Illegal redefinition of function ${func.name}"
        if (prevFunc.isDefined) {
            s"\nPreviously declared on line ${prevFunc.get.pos._1}"
        }
        sb.toString
    }

    override def toString = "Redefined function Error"
}

case class FuncArgumentSizeError(funcName: String, unexpected: Int, expected: Int) extends FunctionError {
    override def formatError = s"Wrong number of arguments provided to function ${funcName}\n" +
        s"unexpected ${unexpected} arguments\nexpected ${expected} arguments"

    override def toString = "Function Call Error"
}

/* Mainly used for errors related to pairs and return */
case class SpecialError(errType: String, msg: String) extends Error {
    override def formatError = msg

    override def toString = errType

}

/* Collects all the errors into a list as semantic checker goes through the AST */
class SemanticErrorCollector(fileName: Option[String] = None, input: String = "") {
    private val semanticErrors: ListBuffer[SemanticError] = ListBuffer.empty
    private val wholeCodeArray: Array[String]             = input.split('\n')

    def addError(node: Node, lines: Error): Unit = {
        semanticErrors += new SemanticError(node, fileName, lines, getCodeSnippet(node.pos))
    }

    def getSemanticErrors: Seq[SemanticError] = semanticErrors.toList

    def formatErrors: String = {
        val builder = new StringBuilder
        builder.append("Errors detected during compilation! Exit code 200 returned.\n")
        for (errorLine <- getSemanticErrors) {
            builder.append(errorLine.formatFullError + "\n\n")
        }
        return builder.toString()
    }

    def getCodeSnippet(position: (Int, Int), linesAbove: Int = 2, linesBelow: Int = 2): Seq[String] = {
        var codelines: ListBuffer[String] = ListBuffer.empty
        val (currentLine, currentCol) = position
        val enableColours = false
        for (lineNum <- currentLine - linesAbove to currentLine + linesBelow) {
            if (lineNum >= 1 && lineNum <= wholeCodeArray.length) {
                if (enableColours && lineNum == currentLine) {
                    codelines.append(Console.RED + "%4d".format(lineNum) + s"|${wholeCodeArray(lineNum - 1)}")
                } else {
                    codelines.append("%4d".format(lineNum) + s"|${wholeCodeArray(lineNum - 1)}")
                }
                

                // Add caret
                if (lineNum == currentLine) {
                    codelines.append("    " + " " + " " * (currentCol - 1) + "^" + (if (enableColours) Console.RESET else ""))
                }
            }
        }
        return codelines.toSeq

    }
}
