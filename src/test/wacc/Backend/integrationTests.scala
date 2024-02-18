package wacc.backend

import org.scalatest.flatspec.AnyFlatSpec
import scala.io.Source
import scala.util.matching.Regex
import scala.util.control.Breaks._
import scala.collection.mutable.ListBuffer
import wacc.semanticChecker
import wacc._
import java.io.File
import java.nio.file.Files
import scala.concurrent.blocking
import java.io.ByteArrayOutputStream
import java.io.PrintWriter

/**
Steps:
    Generate the assembly code locally for all the wacc file listed in the directory specified
    Copy the assembly code into the docker working directory
    Docker compiles the assembly and then produces the output files.
    
  */

class BackendTests extends AnyFlatSpec {
    type Path = String
    type FileContents = List[String]
    type Input = Option[String]
    type Output = List[String]
    type ExitCode = Int
    type FileInfo = (Path, File, Input, Output, ExitCode)


    // def getExitValue(path: String): Option[Int] = {
    //     try {
    //         val fileContents = Source.fromFile(path).getLines()
    //         val exitValRegex = "# \\d+".r
    //         var containsExit = false
    //         var exitVal: Option[Int] = None
    //         for (line <- fileContents) {
    //             if (line.startsWith("# Exit:")) {
    //                 containsExit = true
    //             } else if (containsExit) {
    //                 exitValRegex.findFirstIn(line).map(_.toInt)
    //                 if (exitVal.isDefined) return exitVal
    //             }
    //         }
    //         exitVal
    //     } catch {
    //         case ex: Exception =>
    //             println(s"Error occurred: ${ex.getMessage}")
    //             sys.exit(1)
    //     }
    // }

    "Valid examples" should "match assembler output and exit code" in {
        runTests("wacc_examples/valid/basic/exit") 
    }

    

    private def run(command: Seq[String], inp: Array[Byte], pout: ByteArrayOutputStream): Int = {
        val p = new java.lang.ProcessBuilder(java.util.Arrays.asList(command: _*)).start()
        blocking(p.getOutputStream.write(inp))
        p.getOutputStream.close()
        blocking(pout.write(p.getInputStream().readAllBytes()))
        blocking(p.waitFor())
    }

    def runTests(dirPath: String): Unit = {
        // each "result" is in assembly
        // want to compile it and get its result
        val results = compileAll(dirPath).map { case (asm, info@(input, _, _), path) =>
            val writer = new PrintWriter(new File("out.s"))
            writer.write(asm)
            writer.close()
            val out = new ByteArrayOutputStream
            run(List("./run.sh"), input.getOrElse("").getBytes(), out) // super duper bad
            (out.toString(), info, path)
        }

        // todo: improve this, if output contains required exit code then this will fail 
        // should be better than that
        val failed = results.filter { case (asm, info@(_, output, exit), _) => {
            val asmLines = asm.split("\n").toList
            !((asmLines.last.toInt == exit) && (asmLines.init.equals(output)))
        }}

        val sb = new StringBuilder
        failed.foreach { case (asm, (_, output, exit), path) => 
            sb.append(s"\n======= FAILED: $path =======")
            sb.append(s"\nResult: $asm")
            sb.append(s"\nExpected output: ${output.mkString("\n")}\n")
            sb.append(s"\nExpected exit: ${exit}\n")
            sb.append("\n")
        }

        if (!(failed.isEmpty)) {
            fail(sb.toString())
        }

        info(message=s"${results.size - failed.size}/${results.size} tests passed")
    }

    def compileAll(path: String): List[(String, StdInfo, String)] = {
        getAllFiles(new File(path))
            .toList
            .filter(_.isFile)
            .map(_.toString)
            .map(path => (compileSingle(path), getFields(path), path))
    }
        

    def compileSingle(path: String): String = {
        val input = Source.fromFile(path).mkString

        val syntaxResult = parser.parse(input).toEither
        val semanticResult = semanticChecker.verify(syntaxResult).toOption.get
        val translateResult = codeGenerator.translate(semanticResult)

        translateResult.merge
    }

    def getAllFiles(dir: File): Array[File] = {
        val filesInCurrent = dir.listFiles
        filesInCurrent ++ filesInCurrent.filter(_.isDirectory).flatMap(getAllFiles)
    }

    type StdInfo = (Option[String], List[String], Int)
                            //      in/out/exit
    def getFields(path: String): StdInfo = {
        try {
            val fileContents = Source.fromFile(path).getLines()

            val inputRegex = "# Input: [^\\s]+".r
            val exitRegex = "# \\d+".r
            val outputRegex = "# .+".r


            var nextLineExit = false
            var nextLineOutput = false

            var exitField: Option[String] = None
            var outputField: ListBuffer[String] = ListBuffer.empty
            var inputField: Option[String] = None

            for (line <- fileContents) {
                if (line.startsWith("# Exit:")) {
                    nextLineExit = true
                } else if (line.startsWith("# Output:")) {
                    nextLineOutput = true
                } else if (nextLineExit) {
                    exitRegex.findFirstIn(line).map(_.drop(2))
                    nextLineExit = false
                } else if (nextLineOutput) {
                    outputRegex.findFirstIn(line) match {
                        case None => nextLineOutput = false
                        case Some(output) => outputField.addOne(output.drop(2))
                    }
                } else {
                    inputRegex.findFirstIn(line) match {
                        case None => 
                        case Some(input) => inputField = Some(input.drop(9))
                    }
                }
            }
            
            (inputField, outputField.toList, exitField.map(_.toInt).getOrElse(0))
        } catch {
            case ex: Exception =>
                println(s"Error occurred: ${ex.getMessage}")
                sys.exit(1)
        }
    }
}