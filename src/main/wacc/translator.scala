// package wacc

// import ast._
// import asm._
// import semAst._
// import semanticChecker._

// import scala.collection.mutable.ListBuffer
// import scala.collection.mutable.HashMap

// object codeGenerator {
//     def translate(result: Either[String, SemanticInfo]): String =
//         new Translator(result.toOption.get).toAssembly
// }

// class Translator(semanticInfo: SemanticInfo) {
//     val labelMap: HashMap[String, List[ASMItem]] = HashMap.empty
//     val asmList: List[ASMItem] = translateProgram(semanticInfo.ast)

//     def translateProgram(prog: Prog): List[ASMItem] = {
//         val programHeader = List(
//             Global,
//             Readonly,
//             Text,
//             Label("main"),
//             Push(Rbp),
//             Mov(Rsp, Rbp),
//         )
//         val programBody = prog.stats.flatMap(translateStatement(_))
//         val programFooter = List(
//             Mov(ImmVal(0), Rax),
//             Pop(Rbp),
//             Ret
//         )

//         programHeader ::: programBody ::: programFooter ::: generateLabels
//     }

//     def translateStatement(stat: Stat): List[ASMItem] = {
//         stat match {
//             case Skip() => List.empty
//             case Exit(expr) =>
//                 val exitLabel = Label("_exit")
//                 val mask = -16

//                 addLabel(exitLabel, List(
//                     Push(Rbp),
//                     Mov(Rsp, Rbp),
//                     asm.And(ImmVal(mask), Rsp),
//                     Call(LibFunc.Exit),
//                     Mov(Rbp, Rsp),
//                     Pop(Rbp),
//                     Ret
//                 ))

//                 translateExpression(expr) :::
//                 List(
//                     Mov(Rax, Rdi),
//                     Call(exitLabel)
//                 )
//             case _ => List.empty
//         }
//     }

//     def addLabel(label: Label, asmList: List[ASMItem]) = 
//         labelMap.addOne(label.ident, label :: asmList)
    
//     def generateLabels: List[ASMItem] =
//         labelMap.flatMap(_._2).toList

//     def translateExpression(expr: Expr, targetReg: Operand = Rax): List[ASMItem] = {
//         expr match {
//             case IntVal(num) => 
//                 List(
//                     Mov(ImmVal(num), targetReg)
//                 )
//             case _ => List.empty
//         }
//     }

//     def toAssembly: String = {
//         asmList.map { asm => 
//             asm match {
//                 case section: Section => section.toString()
//                 case label: Label => label.toString() + ":"
//                 case other => "\t" + other.toString()
//             }
//         }.mkString("\n")
//     }
// }