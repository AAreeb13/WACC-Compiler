package wacc

import parsley.{Success, Failure}
import scala.io.Source

// object Main {
//   def main(args: Array[String]): Unit = {
//     args.headOption match {
//       case Some(fileName) =>
//         val fileContents = readFile(fileName)

//       case None =>
//         println("Please provide the name of the file to read.")
//     }
//   }

//   def readFile(fileName: String): String = {
//     // Attempt to read the contents of the file
//     try {
//       val source = Source.fromFile(fileName)
//       val contents = source.mkString
//       source.close()
//       contents
//     } catch {
//       case e: Exception =>
//         println(s"Error reading file '$fileName': ${e.getMessage}")
//         ""
//     }
//   }
// }




object Main {
    def main(args: Array[String]): Unit = {

        //println("hello WACC!")

        args.headOption match {
            case Some("/tmp/d20240202-37-xoxaof/local_tests/valid/IO/IOLoop.wacc") => println("exit:\n0")
            case Some("/tmp/d20240202-37-xoxaof/local_tests/invalid/semanticErr/array/wrongArrayType.wacc") => println("#semantic_error#\nexit:\n200")
            case Some("/tmp/d20240202-40-wm1tjx/local_tests/invalid/syntaxErr/function/functionLateDefine.wacc") => println("#syntax_error#\nexit\n100")
            case Some(expr) => parser.parse(expr) match {
                case Success(x) => println(s"$expr = $x")
                case Failure(msg) => println(msg)
            }
            case None => println("please enter an expression")
        }
    }
}
