package wacc

import globals._
import semanticChecker._

import scala.io.Source
import scala.annotation.switch
import scala.concurrent.blocking

import java.io.ByteArrayOutputStream



object Main {
    /**
      * Parses command line arguments provided to the compiler
      * The available options are:
            -p --only-parse => syntax parse only
            -s --only-typecheck => syntax & semantic checks only
            -x --execute => execute generate assembly code
            -l --colour => coloured output for semantic errors
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
                    case "p" | "-only-parse" =>     syntaxCheckOnly = true
                    case "s" | "-only-typecheck" => semanticCheckOnly = true
                    case "l" | "-colour" =>         enableColours = true
                    case "x" | "-execute" =>        executeAssembly = true
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
        val input = Source.fromFile(path).mkString

        // Syntax Analysis
        val syntaxResult = parser.parse(input)

        syntaxResult match {
            case Left(err) =>
                println(err)
                sys.exit(exitSyntaxErr)
            case Right(ast) =>
                if (syntaxCheckOnly) {
                    println(ast)
                    sys.exit(exitSuccess)
                }
        }

        // Syntax Analysis
        val errorBuilder = SemanticErrorCollector(path, input)
        val semanticResult = semanticChecker.verify(syntaxResult, Some(errorBuilder))

        semanticResult match {
            case Left(err) => 
                println(err)
                sys.exit(exitSemanticErr)
            case Right(SemanticInfo(ast, _, _)) =>
                if (semanticCheckOnly) {
                    println(ast)
                    sys.exit(exitSuccess)
                }
        }

        // Code Generation
        val assemblyResult = codeGenerator.translate(semanticResult)

        println(assemblyResult)

        // Code execution
        if (executeAssembly) {
            generateAndRunExecutable(assemblyResult)
        }

        sys.exit(exitSuccess)
    }

    def generateAndRunExecutable(assembly: String) = {
        if (useDocker) {
            // docker buildx build --platform linux/amd64 -t my-x86-image .
            // docker run -di --platform linux/amd64 --name my-container my-x86-image
            runCommand(Seq("docker", "run", "-di", "--platform", "linux/amd64", "--name", "my-container", "my-x86-image"))
            runCommand(Seq("docker", "ps", "-a"))
        }


        
        val errStream = new ByteArrayOutputStream
        val compileSuccess = runCustomCommand(Seq("gcc", "-x", "assembler", "-", "-z", "noexecstack", "-o", "out"), assembly.getBytes(), errStream)

        if (compileSuccess != 0) {
            println("Compilation failure")
            println(errStream.toString())
        } else {
            val outStream = new ByteArrayOutputStream
            val inStream = Source.stdin.map(_.toByte).toArray
            val actualExit = runCustomCommand(Seq("./out"), inStream, outStream)
            println("Output:")
            println(outStream.toString())
            println(s"Executable finished with exit code $actualExit")

            if (!useDocker) {
                runCustomCommand(Seq("rm", "-f", "./out"))
            }
        }

        if (useDocker) {
            runCommand(Seq("docker", "rm", "-f", "my-container"))
        }
    }

    def runCustomCommand(command: Seq[String],
                         inp: Array[Byte] = Array.emptyByteArray,
                         pout: ByteArrayOutputStream = new ByteArrayOutputStream): Int = {
        if (useDocker) {
            runCommand(Seq("docker", "exec", "-i", "my-container") ++ command, inp, pout)
        } else {
            runCommand(command, inp, pout)
        }
    }


    def runCommand(command: Seq[String],
                   inp: Array[Byte] = Array.emptyByteArray,
                   pout: ByteArrayOutputStream = new ByteArrayOutputStream): Int = {
        val p = new java.lang.ProcessBuilder(java.util.Arrays.asList(command: _*)).start()
        blocking(p.getOutputStream.write(inp))
        p.getOutputStream.close()
        blocking(pout.write(p.getInputStream().readAllBytes()))
        blocking {
            pout.write(Console.RED.getBytes())
            pout.write(p.getErrorStream().readAllBytes())
            pout.write(Console.RESET.getBytes())
        }
        blocking(p.waitFor())
    }

}
