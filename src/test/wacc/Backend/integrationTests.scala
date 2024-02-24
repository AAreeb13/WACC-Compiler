package wacc.backend.integration

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
import wacc.globals._


class Tests extends AnyFlatSpec {
    type Path = String
    type FileContents = String
    type Input = Option[String]
    type Output = List[String]
    type ExitCode = Int
    type ResultInfo = (Input, Output, ExitCode)
    type FileInfo = (Path, FileContents, ResultInfo)

    val examplesDir = "wacc_examples/valid"
    val asmDir = "backend_output"

    /**
      * Compiles all the files in a given path
      */
    "Valid examples" should "match assembler output and exit code" in {
        performTests(examplesDir) 
    }

    def performTests(dir: Path) = {
        val assembledFiles = assembleAll(dir)

        if (System.getProperty("os.arch").equals("aarch64")) {
            useDocker = true
        }

        if (useDocker) {
            // docker buildx build --platform linux/amd64 -t my-x86-image .
            // docker run -di --platform linux/amd64 --name my-container my-x86-image
            //runCommand(Seq("docker", "buildx", "build", "--platform", "linux/amd64", "-t",  "my-x86-image", "."))
            runCommand(Seq("docker", "run", "-di", "--platform", "linux/amd64", "--name", "my-container", "my-x86-image"))
        }

        val failed = assembledFiles.map { case fileInfo@(examplePath, asmOutput, (in, expectedOut, expectedExit)) =>
            // gcc -x assembler - -z noexecstack -o out (input is piped so no need to create .s file)
            runCommandWithDocker(Seq("gcc", "-x", "assembler", "-", "-z", "noexecstack", "-o", "out"), asmOutput.getBytes())

            val outStream = new ByteArrayOutputStream 
            val actualExit = runCommandWithDocker(Seq("./out"), in.fold(Array.emptyByteArray)(_.getBytes()), outStream)
            val actualOut: Output = outStream.toString().split("\n").toList.filter(!_.isEmpty()) // alternatively do .filter(!_.isEmpty()) but sometimes empty lines are important

            (examplePath, asmOutput, in, expectedOut, expectedExit, actualOut, actualExit)
        }.filter { case (_, _, _, expectedOut, expectedExit, actualOut, actualExit) =>
            !((actualExit == expectedExit) && (actualOut.equals(expectedOut))) 
        }


        // Cleanup artifacts, in the case of docker it is the container otherwise remove the ./out executable
        if (useDocker) {
            runCommand(Seq("docker", "rm", "-f", "my-container"))
        } else {
            runCommand(Seq("rm", "-f", "./out"))
        }

        // Format errors
        val sb = new StringBuilder
        failed.foreach { case (path, asmRaw, in, expectedOut, expectedExit, actualOut, actualExit) => 
            sb.append(s"\n======= FAILED: $path =======")
            sb.append(s"\nActual output: ${actualOut}")
            sb.append(s"\nExpected output: ${expectedOut}")
            sb.append(s"\nActual exit: ${actualExit}")
            sb.append(s"\nExpected exit: ${expectedExit}")
            sb.append(s"\nSupplied Input: ${in}")
            sb.append(s"\nAssembler Output:\n${asmRaw}\n")
        }

        if (!(failed.isEmpty)) {
            sb.append(s"\n${assembledFiles.size - failed.size}/${assembledFiles.size} tests passed")
            fail(sb.toString())
        } else {
            info(message=s"${assembledFiles.size - failed.size}/${assembledFiles.size} tests passed")
        }
    }

    def runCommandWithDocker(command: Seq[String],
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
                    exitField = exitRegex.findFirstIn(line).map(_.drop(2))
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