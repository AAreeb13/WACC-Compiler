package wacc

import wacc.ast._
import scala.collection.mutable.ListBuffer
import wacc.asmIR._
import scala.collection.mutable.HashMap

object codeGenerator {
    def translate(ast: Node, symbolTable: SymbolTable): Either[String, String] = {
        ast match {
            case prog: Prog => Right(new Translator(prog, symbolTable).toAssembly)
            case _ => Left("Invalid AST type for error generation")
        }
    }

    // def translate(ast: Node) = ast match {
    //     case Add(op1, op2) => op1 match {
            
    //     }
    // }
}

class Translator(prog: Prog, val symbolTable: SymbolTable) {
    val labelMap: HashMap[String, List[ASMItem]] = HashMap.empty
    val asmList: List[ASMItem] = translateProgram(prog)

    def translateProgram(prog: Prog): List[ASMItem] = {
        val programHeader = List(
            Global,
            Readonly,
            Text,
            Label("main"),
            Push(Rbp),
            Mov(Rsp, Rbp),
        )
        val programBody = prog.stats.flatMap(translateStatement(_))
        val programFooter = List(
            Mov(ImmVal(0), Rax),
            Pop(Rbp),
            Ret
        )

        programHeader ::: programBody ::: programFooter ::: generateLabels
    }

    def translateStatement(stat: Stat): List[ASMItem] = {
        stat match {
            case Skip() => List.empty
            case Exit(expr) =>
                val exitLabel = Label("_exit")
                val mask = -16

                addLabel(exitLabel, List(
                    Push(Rbp),
                    Mov(Rsp, Rbp),
                    asmIR.And(ImmVal(mask), Rsp),
                    Call(LibFunc.Exit),
                    Mov(Rbp, Rsp),
                    Pop(Rbp),
                    Ret
                ))

                translateExpression(expr) :::
                List(
                    Mov(Rax, Rdi),
                    Call(exitLabel)
                )
            case _ => List.empty
        }
    }

    def addLabel(label: Label, asmList: List[ASMItem]) = 
        labelMap.addOne(label.ident, label :: asmList)
    
    def generateLabels: List[ASMItem] =
        labelMap.flatMap(_._2).toList

    def translateExpression(expr: Expr, targetReg: Operand = Rax): List[ASMItem] = {
        expr match {
            case IntVal(num) => 
                List(
                    Mov(ImmVal(num), targetReg)
                )
            case _ => List.empty
        }
    }

    def toAssembly: String = {
        asmList.map { asm => 
            asm match {
                case section: Section => section.toString()
                case label: Label => label.toString() + ":"
                case other => "\t" + other.toString()
            }
        }.mkString("\n")
    }
}