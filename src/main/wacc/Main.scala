package wacc

import scala.io.Source

object Main {
    val exitSuccess     = 0;
    val exitSyntaxErr   = 100;
    val exitSemanticErr = 200;

    def main(args: Array[String]): Unit = {
        val input = args.headOption match {
            case None =>
            println("Command line input not found, defaulting to file input (in.txt)")
            Source.fromFile("in.txt").mkString
            case Some(path) =>
                Source.fromFile(path).mkString
        }

        // Syntax Analysis

        var result = parser.parse(input).toEither
        result match {
            case Left(err) =>
                println(err)
                sys.exit(exitSyntaxErr)

            case Right(_) =>
                //result = semanticChecker.verify(result)
        }

        // Semantic Analysis

        result match {
            case Left(err) =>
                println(s"$input\n$err")
                sys.exit(exitSemanticErr)

            case Right(output) =>
                println(s"$input => $output")
                sys.exit(exitSuccess)
        }

    }
}
