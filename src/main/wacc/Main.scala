package wacc

import scala.io.Source

object Main {
    val exitSuccess     = 0;
    val exitSyntaxErr   = 100;
    val exitSemanticErr = 200;

    def main(args: Array[String]): Unit = {
        val path = args.headOption.getOrElse("in.txt")

        // val input = args.headOption match {
        //     case None =>
        //     println("Command line input not found, defaulting to file input (in.txt)")
        //     Source.fromFile("in.txt").mkString
        //     case Some(path) =>
        //         Source.fromFile(path).mkString
        // }

        // Syntax Analysis

        //var result = parser.parseFile(path).toEither
        val input = Source.fromFile(path).mkString
        var result = parser.parse(input).toEither
        val ec = new SemanticErrorCollector(Some(path), input)
        result match {
            case Left(err) =>
                println(err)
                sys.exit(exitSyntaxErr)

            case Right(_) =>
                result = semanticChecker.verify(result, Some(ec))
        }
        
        // Semantic Analysis

        result match {
            case Left(err) =>
                println(s"$err")
                sys.exit(exitSemanticErr)

            case Right(output) =>
                println(s"$output")
                sys.exit(exitSuccess)
        }

    }
}
