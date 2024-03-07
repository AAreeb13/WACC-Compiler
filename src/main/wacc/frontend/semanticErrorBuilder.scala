package wacc
import ast._
import parsley.errors.ErrorBuilder
import scala.collection.mutable.ListBuffer
import parsley.position
import globals._

/**
  * class which abstracts out the components which are same across all errors
  * @param node the node which the occurred at
  * @param fileName optional name of the file if provided
  * @param errorType error type from the classes defined below
  * @param codeSnippet optional code snipped if code was provided
  * @param msg an optional message for your error
  */
case class SemanticError(node: Node, fileName: Option[String], errorType: Error, codeSnippet: Seq[String], msg: String = "") {
    def formatFullError: String = {
        val formatStr = new StringBuilder
        formatStr.append(errorType.toString())
        if (fileName.isDefined)
            formatStr.append(s" in ${fileName.get} ${node.pos}")
        formatStr.append("\n" + errorType.formatError + "\n")
        formatStr.append(msg + "\n")
        codeSnippet.foreach { line =>
            formatStr.append(s"${line}\n")
        }
        return formatStr.toString
    }
}

/**
  * Abstract error trait which all other errors implement
  */
sealed trait Error {
    def formatError: String
}

/**
  * Regular type mismatch
  *
  * @param unexpected the name of the type that was given 
  * @param expected the name of the type that was expected, can be a custom message
  */
case class TypeError(unexpected: String, expected: String) extends Error {
    override def formatError = s"unexpected ${unexpected}\nexpected ${expected}"
    override def toString = "Type Error"
}

/**
  * Regular scope error
  */
sealed trait ScopeError extends Error {
    override def toString = "Scope Error"
}

/**
  * When two variables have the same name in the same scope
  *
  * @param node the current node which caused the error
  * @param prevNode the node for initial declaration of the node, which provides lineInfo
  */
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

/**
  * For variables which have not been initialised/declared yet
  *
  * @param node The node which has not been declared
  */
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

/**
  * Name of the undefined function (in a function call)
  *
  * @param funcName
  */
case class UndefinedFuncError(funcName: String) extends FunctionError {

    override def formatError = s"${funcName} has not been defined"

    override def toString = "Undefined Function Error"

}

/**
  * Redeclaration of a function (note: overloading has not been supported yet)
  *
  * @param func the redeclared function
  * @param prevFunc the previous declaration of the function
  */
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

/**
  * Function call where arguments don't have the same length
  *
  * @param funcName name of the function
  * @param unexpected given number of arguments
  * @param expected expected number of arguments
  */
case class FuncArgumentSizeError(funcName: String, unexpected: Int, expected: Int) extends FunctionError {
    override def formatError = s"Wrong number of arguments provided to function ${funcName}\n" +
        s"unexpected ${unexpected} arguments\nexpected ${expected} arguments"

    override def toString = "Function Call Error"
}

/**
  * Errors that don't fit the traditional format. E.g to do with pairs and other specific scenarios
  */
case class SpecialError(errType: String, msg: String) extends Error {
    override def formatError = msg
    override def toString = errType

}

/**
  * Collects all the errors into a list as semantic checker goes through the AST
  *
  * @param fileName if input comes from a file then the name of the file
  * @param input if input is not in AST form, the actual text itself
  */
class SemanticErrorCollector(fileName: Option[String] = None, input: String = "") {
    private val semanticErrors: ListBuffer[SemanticError] = ListBuffer.empty
    private val wholeCodeArray: Array[String]             = input.split('\n')

    // Returns if errors are present in the collector
    def containsError: Boolean = semanticErrors.isEmpty

    // Add error with specified node and message
    def addError(node: Node, lines: Error, optionalMessage: String = ""): Unit = {
        semanticErrors += new SemanticError(node, fileName, lines, getCodeSnippet(node.pos), optionalMessage)
    }

    private def getSemanticErrors: Seq[SemanticError] = semanticErrors.toList

    // Returns a string of the all the errors
    def formatErrors: String = {
        val builder = new StringBuilder
        builder.append(s"Errors detected during compilation! Exit code ${exitSemanticErr} returned.\n")
        for (errorLine <- getSemanticErrors) {
            builder.append(errorLine.formatFullError + "\n")
        }
        return builder.toString()
    }

    // Returns the specified snippet of code at the error location
    private def getCodeSnippet(position: (Int, Int), linesAbove: Int = 2, linesBelow: Int = 2): Seq[String] = {
        var codelines: ListBuffer[String] = ListBuffer.empty
        val (currentLine, currentCol) = position
        for (lineNum <- currentLine - linesAbove to currentLine + linesBelow) {
            // Formats the errors with line numbers, colours and some padding
            if (lineNum >= 1 && lineNum <= wholeCodeArray.length) {
                if (enableColours && lineNum == currentLine) {
                    codelines.append(Console.YELLOW + "%4d".format(lineNum) + s"|${wholeCodeArray(lineNum - 1)}" + Console.RESET)
                } else {
                    codelines.append("%4d".format(lineNum) + s"|${wholeCodeArray(lineNum - 1)}")
                }
                

                // Add caret at the column
                if (lineNum == currentLine) {
                    if (enableColours)
                        codelines.append(Console.YELLOW + "    " + " " + " " * (currentCol - 1) + "^" + Console.RESET)
                    else
                        codelines.append("    " + " " + " " * (currentCol - 1) + "^")
                }
            }
        }
        return codelines.toSeq

    }
}

object SemanticErrorCollector {
    def apply(path: String, contents: String): SemanticErrorCollector = 
        new SemanticErrorCollector(Some(path), contents)
}
