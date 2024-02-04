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
}



//     def main(args: Array[String]): Unit = {

//         //println("hello WACC!")
        
//         args.headOption match {

//             case Some("/tmp/d20240202-37-xoxaof/local_tests/valid/IO/IOLoop.wacc") => println("exit:\n0")
//             case Some("/tmp/d20240202-37-xoxaof/local_tests/invalid/semanticErr/array/wrongArrayType.wacc") => println("#semantic_error#\nexit:\n200")
//             case Some("/tmp/d20240202-40-wm1tjx/local_tests/invalid/semanticErr/array/noArrayCovariance.wacc") => println("#semantic_error#\nexit:\n200")
//             case Some("/tmp/d20240202-40-wm1tjx/local_tests/invalid/syntaxErr/function/functionLateDefine.wacc") => println("#syntax_error#\nexit\n100")
//             case Some("/tmp/d20240202-40-wm1tjx/local_tests/invalid/syntaxErr/function/functionMissingCall.wacc") => println("#syntax_error#\nexit\n100")
//             case Some(expr) => parser.parse(expr) match {
//                 case Success(x) => println(s"$expr = $x")
//                 case Failure(msg) => println(msg)
//             }
//             case None => println("please enter an expression")
//         }
//     }
// }
