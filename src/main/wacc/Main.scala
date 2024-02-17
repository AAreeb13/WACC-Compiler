package wacc

import scala.io.Source
import globals._
import scala.annotation.switch

object Main {
    /**
      * This function parses command line arguments that are provided to the compiler
      * Currently the available options are:
            -p --only-parse => syntax parse only
            -s --only-typecheck => syntax & semantic only
            -l --colour => coloured output for errors
      */
    def parseArguments(args: Array[String]): String = {
        var filenameOption: Option[String] = None

        // fallback method in case invalid options are provided
        def printErrAndLeave = {
            println("unsupported operation, usage: scala-cli . -- [options] <path>?")
            sys.exit(exitInvalidOptions)
        }

        var i = 0
        args.zipWithIndex.foreach { case (arg, i) =>
            if (arg.startsWith("-")) {
                arg.tail match {
                    case "p" | "-only-parse" =>
                        fullTypeCheck = false
                    case "s" | "-only-typecheck" =>
                        compile = false
                    case "l" | "-colour" =>
                        enableColours = true
                    case _ => printErrAndLeave
                }
            } else if (i != args.length - 1) {
                printErrAndLeave
            } else {
                filenameOption = Some(arg)
            }
        }

        filenameOption.getOrElse("in.txt")
    }

    def main(args: Array[String]): Unit = {
        val path = parseArguments(args)
        
        // Syntax Analysis

        //var result = parser.parseFile(path).toEither
        val input = Source.fromFile(path).mkString
        var result = parser.parse(input).toEither

        // Create the semantic error collector for error generation
        result match {
            case Left(err) =>
                println(err)
                sys.exit(exitSyntaxErr)
            case Right(ast) =>
                if (fullTypeCheck) {
                    val ec = new SemanticErrorCollector(Some(path), input)
                    semanticChecker.verify(result, Some(ec)) match {
                        case Left(err) =>
                            println(err)
                            sys.exit(exitSemanticErr) 
                        case Right((ast, st)) =>
                            if (compile) {
                                codeGenerator.translate(ast, st) match {
                                    case Left(err) =>
                                        println(err)
                                        sys.exit(exitRuntimeErr) 
                                    case Right(asm) =>
                                        println(asm)
                                        sys.exit(exitSuccess)
                                }
                            } else {
                                println(ast)
                                sys.exit(exitSuccess)
                            }
                    }
                } else {
                    println(ast)
                    sys.exit(exitSuccess)
                }
        }


    }
}
