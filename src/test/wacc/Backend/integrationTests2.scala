package wacc.backend.test

import org.scalatest.flatspec.AnyFlatSpec
import java.io.File
import scala.io.Source
import wacc.parser
import wacc.semanticChecker
import wacc.codeGenerator
import scala.collection.mutable.ListBuffer
import java.nio.file.Paths
import java.nio.file.Files
import java.io.PrintWriter
import scala.concurrent.blocking
import java.io.ByteArrayOutputStream


class Tests extends AnyFlatSpec {
    type Path = String
    type FileContents = String
    type Input = Option[String]
    type Output = List[String]
    type ExitCode = Int
    type ResultInfo = (Input, Output, ExitCode)
    type FileInfo = (Path, FileContents, ResultInfo)

    val examplesDir = "wacc_examples/valid/basic"
    val asmDir = "backend_output"
    val useDocker = true

    /**
      * Compiles all the files in a given path
      */
    
    "Valid examples" should "match assembler output and exit code" in {
        performTests(examplesDir) 
    }

    // def dockerCommand(command: Seq[String])(implicit containerID: String = "") =
    //     Seq("docker", "exec", "--rm", "-i", "gumjoe/wacc-ci-scala:slim") ++ command

    def performTests(dir: Path) = {
        println("Starting asm generation")

        val assembledFiles = assembleAll(dir)

        println("Generated asm files")
        
        if (useDocker) {
            println("Generating docker container")
            val outputStream = new ByteArrayOutputStream
            runCommand(Seq("docker", "create", "-i", "--platform", "linux/amd64", "gumjoe/wacc-ci-scala:x86"),
                       Array.emptyByteArray,
                       outputStream)
            implicit val containerID = outputStream.toString().trim()
            println("Created docker container")

            val failed = assembledFiles.map { case fileInfo@(examplePath, asmOutput, (in, expectedOut, expectedExit)) =>
                runCommand(Seq("docker", "exec", containerID, "gcc", "-x", "assembler", "-", "-z", "noexecstack", "-o", "out"), asmOutput.getBytes())
                val outStream = new ByteArrayOutputStream
                runCommand(Seq("docker", "exec", containerID, "./out"), in.fold(Array.emptyByteArray)(_.getBytes()), outStream)
                runCommand(Seq("docker", "exec", containerID, "echo $?"), Array.emptyByteArray, outStream)
                val rawOut = outStream.toString().split("\n").toList
                val actualOut = rawOut.init
                val actualExit = rawOut.last.toIntOption.getOrElse(0)
                (fileInfo, actualOut, actualExit)
            }.filter { case ((_, _, (_, expectedOut, expectedExit)), actualOut, actualExit) =>
                !((actualExit == expectedExit) && (actualOut.equals(expectedOut))) 
            }

            runCommand(Seq("docker", "rm", "-f", containerID))

            println("Removed docker container")

            printResults(assembledFiles.length, failed)
        } else {
            //gcc -o out -z noexecstack out.s
            val failed = assembledFiles.map { case fileInfo@(examplePath, asmOutput, (in, expectedOut, expectedExit)) =>
                val outStream = new ByteArrayOutputStream
                runCommand(Seq("./temp.sh"), in.fold(Array.emptyByteArray)(_.getBytes()), outStream)
                val rawOut = outStream.toString().split("\n").toList
                val actualOut = rawOut.init
                val actualExit = rawOut.last.toIntOption.getOrElse(0)
                (fileInfo, actualOut, actualExit)
            }.filter { case ((_, _, (_, expectedOut, expectedExit)), actualOut, actualExit) =>
                !((actualExit == expectedExit) && (actualOut.equals(expectedOut))) 
            }

            printResults(assembledFiles.length, failed)
        }

    }

    def printResults(total: Int, failed: List[(FileInfo, Output, ExitCode)]) = {
        val sb = new StringBuilder
        failed.foreach { case ((path, asmRaw, (in, expectedOut, expectedExit)), actualOut, actualExit) => 
            sb.append(s"\n======= FAILED: $path =======")
            sb.append(s"\nActual output:\n${actualOut.mkString("\n")}")
            sb.append(s"\nExpected output:\n${expectedOut.mkString("\n")}")
            sb.append(s"\nActual exit: ${actualExit}")
            sb.append(s"\nExpected exit: ${expectedExit}")
            sb.append(s"\nSupplied Input: \"${in.getOrElse("")}\"")
            sb.append(s"\nAssembler Output:\n$asmRaw\n")
            sb.append("\n")
        }

        if (!(failed.isEmpty)) {
            sb.append(s"${total - failed.size}/${total} tests passed")
            fail(sb.toString())
        }

        info(message=s"${total - failed.size}/${total} tests passed")
    }

    def runCommand(command: Seq[String], inp: Array[Byte] = Array.emptyByteArray, pout: ByteArrayOutputStream = new ByteArrayOutputStream): Int = {
        val p = new java.lang.ProcessBuilder(java.util.Arrays.asList(command: _*)).start()
        blocking(p.getOutputStream.write(inp))
        p.getOutputStream.close()
        blocking(pout.write(p.getInputStream().readAllBytes()))
        blocking(p.waitFor())
    }

    def assembleAll(path: String): List[FileInfo] = {
        getAllFiles(new File(path))
            .toList
            .filter(_.isFile)
            .map(_.toString)
            .map { path => 
                val expectedResults = getFields(path)
                val rawAssembly = assembleSingle(path)

                // if (useDocker)
                //     createAssemblyFile(path, rawAssembly)

                (path, rawAssembly, expectedResults)
            }
    }

    /**
      * Create a temporary assembly output file for a given file
      */
    def createAssemblyFile(path: Path, rawAssembly: FileContents): Unit = {
        // Create the corresponding directory for assembly output
        val asmPath = path.replace(examplesDir, asmDir)
        val parentDir = Paths.get(asmPath).getParent()
        if (!Files.exists(parentDir))
            Files.createDirectories(parentDir)

        // Write the string to the file
        val writer = new PrintWriter(asmPath)
        try {
            writer.write(rawAssembly)
        } finally {
            writer.close()
        }
    }

    def assembleSingle(path: Path): FileContents = {
        val input = Source.fromFile(path).mkString

        val syntaxResult = parser.parser.parse(input).toEither
        val semanticResult = semanticChecker.verify(syntaxResult).toOption.get
        val translateResult = codeGenerator.translate(semanticResult)

        translateResult.merge
    }

    /**
      * Gets all the non-directory files recursively inside a directory
      */
    def getAllFiles(dir: File): Array[File] = {
        val filesInCurrent = dir.listFiles
        filesInCurrent ++ filesInCurrent.filter(_.isDirectory).flatMap(getAllFiles)
    }

    /**
      * Extracts all the results for a given file
      */
    def getFields(path: Path): ResultInfo = {
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