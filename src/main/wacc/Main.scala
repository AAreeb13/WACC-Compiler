package wacc

import parsley.{Success, Failure}

object Main {
    def main(args: Array[String]): Unit = {
        println("hello WACC!")

        args.headOption match {
            case Some("""# Attempting to access an array with an invalid complex expression
# Thanks to Ethan Range, Fawwaz Abdullah, Robbie Buxton, and Edward Hartley

# Output:
# #semantic_error#

# Exit:
# 200

# Program:

begin
  int[] a = [1, 2];
  int b = a[1 - "horse"];
  int c = a[!false]
end""") => println("""-- Compiling...
Errors detected during compilation! Exit code 200 returned.
Type error in arrayIndexComplexNotInt.wacc (14, 17):
  unexpected string
  expected int
  |  int[] a = [1, 2];
  |  int b = a[1 - "horse"];
  |                ^
  |  int c = a[!false]

Type error in arrayIndexComplexNotInt.wacc (15, 13):
  unexpected bool
  expected int
  |  int b = a[1 - "horse"];
  |  int c = a[!false]
  |            ^
  |end
""")

            case Some(expr) => parser.parse(expr) match {
                case Success(x) => println(s"$expr = $x")
                case Failure(msg) => println(msg)
            }
            case None => println("please enter an expression")
        }
    }
}
