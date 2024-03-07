package wacc.integration

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._

import wacc._
import astFactory._
import java.io.File
import java.nio.file.Files
import scala.io.Source
import globals._
import semanticChecker.SemanticInfo


class FrontendIntegrationTest extends AnyFlatSpec {
    "Valid examples" should "compile successfully" in {
        runTests("wacc_examples/valid", exitSuccess) 
    }

    "Syntax error examples" should "produce a syntax error" in {
        runTests("wacc_examples/invalid/syntaxErr", exitSyntaxErr) 
    }

    "Semantic error examples" should "produce a semantic error" in {
        runTests("wacc_examples/invalid/semanticErr", exitSemanticErr) 
    }

    def runTests(path: String, expected: Int): Unit = {
        val directoryAsFile = new File(path)

        val results = compileAll(path)
        val failed = results.filter{ case (_, _, actual) => actual != expected }

        val sb = new StringBuilder
        failed.foreach { case (path, output, errCode) => 
            sb.append(s"\n======= FAILED: $path =======")
            sb.append(s"\nResult: $errCode")
            sb.append(s"\nExpected: $expected\n")
            sb.append(aBunchOfDashes(path))
            sb.append(output)
            sb.append("\n\n")
        }

        if (!(failed.isEmpty || ((expected == exitSemanticErr) && semanticCheckOnly))) {
            fail(sb.toString())
        }

        info(message=s"${results.size - failed.size}/${results.size} tests passed")
    }

    def compileAll(path: String): List[(String, String, Int)] = {
        return getAllFiles(new File(path))
            .toList
            .filter(_.isFile)
            .map(_.toString)
            .map(compileSingle(_))
        }

    // dont worry its all just aesthetics
    def aBunchOfDashes(path: String) = ("-" * (24 + path.size)) ++ "\n"

    def compileSingle(path: String): (String, String, Int) = {
        val input = Source.fromFile(path).mkString

        val syntaxResult = parser.parse(input)

        syntaxResult match {
            case Left(err) =>
                return (path, input ++ aBunchOfDashes(path) ++ err, exitSyntaxErr)

            case Right(_) =>
        }

        val errorCollector = Some(new SemanticErrorCollector(Some(path), input))
        val semanticResult = semanticChecker.verify(syntaxResult, errorCollector)

        semanticResult match {
            case Left(err) =>
                return (path, input ++ aBunchOfDashes(path) ++ err, exitSemanticErr)

            case Right(SemanticInfo(ast, _, _)) => 
                return (path, ast.toString, exitSuccess)
        }    

    }

    def getAllFiles(dir: File): Array[File] = {
        val filesInCurrent = dir.listFiles
        filesInCurrent ++ filesInCurrent.filter(_.isDirectory).flatMap(getAllFiles)
    }
}