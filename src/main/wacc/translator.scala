package wacc

import wacc.ast._
import scala.collection.mutable.ListBuffer
import wacc.asmIR._
import scala.collection.mutable.HashMap
import scala.language.implicitConversions

object codeGenerator {
    def translate(astInfo: (Node, List[SymbolTable])): Either[String, String] = translate(astInfo._1, astInfo._2)
    def translate(ast: Node, symbolTables: List[SymbolTable]): Either[String, String] = {
        ast match {
            case prog: Prog =>
                Right(new Translator(prog, symbolTables).toAssembly)
            case _ => Left("Invalid AST type for error generation")
        }
    }

    // def translate(ast: Node) = ast match {
    //     case Add(op1, op2) => op1 match {
            
    //     }
    // }
}

class Translator(prog: Prog, val symbolTables: List[SymbolTable]) {
    val labelMap: HashMap[String, List[ASMItem]] = HashMap.empty

    val topLevelTable = symbolTables.head
    val functionTables = symbolTables.tail

    val readOnlyList: ListBuffer[ASMItem] = ListBuffer.empty

    val asmList: List[ASMItem] = translateProgram(prog)

    def translateProgram(prog: Prog): List[ASMItem] = {
        val programHeader = List(
            Text,
            Label("main"),
            Push(Rbp),
            Mov(Rsp, Rbp),
        )
        val programBody = prog.stats.flatMap(translateStatement(_))

        val popVariablesInScope = topLevelTable.table.map(_ => Pop(Rax)).toList

        val programFooter = List(
            Mov(ImmVal(0), Rax),
            Pop(Rbp),
            Ret
        )

        List(Global, Readonly) ::: 
        readOnlyList.toList :::
        programHeader :::
        programBody :::
        popVariablesInScope :::
        programFooter :::
        generateLabels
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
            case AssignNew(declType, _, rvalue) =>
                translateRValue(rvalue) :::
                List(
                    Push(Rax, InstrSize.QWord)
                )
            case _ => List.empty
        }
    }

    implicit def typeToSize(declType: Type): InstrSize.Size = declType match {
        case ArrayType(_) => InstrSize.QWord
        case BoolType() => InstrSize.Byte
        case CharType() => InstrSize.Byte
        case IntType() => InstrSize.DWord
        case StringType() => InstrSize.QWord
        case ErasedPair() => InstrSize.QWord
        case PairType(_, _) => InstrSize.QWord
    }

    def translateRValue(rvalue: RValue, targetReg: Operand = Rax): List[ASMItem] = rvalue match {
        case expr: Expr => translateExpression(expr, targetReg)
        case _ => List.empty
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
            case CharVal(x) => 
                List(
                    Mov(ImmVal(x.toInt), targetReg)
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