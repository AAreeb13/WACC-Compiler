package wacc

import wacc.ast._
import scala.collection.mutable.ListBuffer
import wacc.asmIR._
import scala.collection.mutable.HashMap

object translator {
    def translateProgram(prog: Prog)(implicit symbolTable: SymbolTable): List[ASMItem] = {
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

        programHeader ::: programBody ::: programFooter
    }

    def translateStatement(stat: Stat)(implicit symbolTable: SymbolTable): List[ASMItem] = {
        stat match {
            case Skip() => List.empty
            case Exit(expr) =>
                addLabel("_exit")
                translateExpression(expr)
                List(
                    
                )
            case _ => List.empty
        }
    }

    def addLabel(name: String, asmList: List[ASMItem]) = ???

    def translateExpression(expr: Expr, targetReg: Operand = Rax)(implicit symbolTable: SymbolTable): List[ASMItem] = {
        expr match {
            case IntVal(num) => 
                List(
                    Mov(ImmVal(num), targetReg)
                )
            case _ => List.empty
        }
    }

    def translate(ast: Node)(implicit symbolTable: SymbolTable): Either[String, String] = {
        ast match {
            case prog: Prog =>
                Right(new Translator(prog, symbolTable).toAssembly)
                Right(toAssembly(translateProgram(prog)))
            case _ => Left("Invalid AST type for error generation")
        }
    }

    // def translate(ast: Node) = ast match {
    //     case Add(op1, op2) => op1 match {
            
    //     }
    // }

    def toAssembly(asmList: List[ASMItem]): String = asmList.map { 
        asm => asm match {
            case section: Section => section.toString()
            case label: Label => label.toString()
            case other => "\t" + other.toString()
        }
    }.mkString("\n")
}

class Translator() {
    val labels: Map[Label, List[ASMItem]] = HashMap.empty
    val asmList: List[ASMItem]
}