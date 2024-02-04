package wacc

import parsley.{Failure, Success}
import scala.io.Source

object Main {
    val exitSuccess     = 0;
    val exitSyntaxErr   = 100;
    val exitSemanticErr = 200;

    def main(args: Array[String]): Unit = {

        val input = args.headOption.getOrElse {
            println("Command line input not found, defaulting to file input (in.txt)")
            Source.fromFile("in.txt").mkString
        }

        val result = parser.parse(input)
        result match {
            case Failure(err)    => println(err); sys.exit(exitSyntaxErr)
            case Success(output) => println(s"$input => $output"); sys.exit(exitSuccess)
        }
    }
}
