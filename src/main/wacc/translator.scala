package wacc

import wacc.ast._
import scala.collection.mutable.ListBuffer
import wacc.asmIR._

object translator {
    def translateProgram(prog: Prog)(implicit symbolTable: SymbolTable): List[ASMItem] = {
        Global :: Nil
    }

    def translate(ast: Node)(implicit symbolTable: SymbolTable): Either[String, String] = {
        ast match {
            case prog: Prog => Right(toAssembly(translateProgram(prog)))
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