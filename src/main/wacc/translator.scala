package wacc

import wacc.ast._
import wacc.asmIR._
import wacc.asmIR.ComparisonType._
import wacc.asmIR.InstructionSize._
import wacc.asmIR.RegisterNames._

import scala.collection.mutable.ListBuffer
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
            Push(Reg(RegisterNames.Rbp)),
            Mov(Reg(Rsp), Reg(Rbp)),
        )
        val programBody = prog.stats.flatMap(translateStatement(_))

        val popVariablesInScope = topLevelTable.table.map(_ => Pop(Reg(Rax))).toList

        val programFooter = List(
            Mov(ImmVal(0), Reg(Rax)),
            Pop(Reg(Rbp)),
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
                    Push(Reg(Rbp)),
                    Mov(Reg(Rsp), Reg(Rbp)),
                    asmIR.And(ImmVal(mask), Reg(Rsp)),
                    Call(LibFunc.Exit),
                    Mov(Reg(Rbp), Reg(Rsp)),
                    Pop(Reg(Rbp)),
                    Ret
                ))

                translateExpression(expr) :::
                List(
                    Mov(Reg(Rax), Reg(Rdi)),
                    Call(exitLabel)
                )

            case AssignNew(declType, _, rvalue) =>
                translateRValue(rvalue) :::
                List(
                    Push(Reg(Rax), QWord)
                )
            
            case p@Print(expr) => 
                translateExpression(expr) :::
                List(
                    Mov(Reg(Rax), Reg(Rdi))
                ) :::
                handlePrint(p.enclosingType)
                
            case _ => List.empty
        }
    }

    def handlePrint(format: SemType): List[ASMItem] = {
        format match {
            case SemInt =>
                val printIntLabel = Label("_printi")
                val printIntStringLabel = Label(".L._printi_str0")

                readOnlyList.addOne(StringDecl("%d", printIntStringLabel))

                addLabel(printIntLabel, List(
                    Push(Reg(Rbp)),
                    Mov(Reg(Rsp), Reg(Rbp)),
                    asmIR.And(ImmVal(-16), Reg(Rsp)),
                    Mov(Reg(Rsi, DWord), Reg(Rsi, DWord), DWord),
                    Lea(Mem(Reg(Rip), printIntStringLabel), Reg(Rdi)),
                    Mov(ImmVal(0), Reg(Rax, Byte), Byte),
                    Call(LibFunc.Printf),
                    Mov(ImmVal(0), Reg(Rdi)),
                    Call(LibFunc.Flush),
                    Mov(Reg(Rbp), Reg(Rsp)),
                    Pop(Reg(Rbp)),
                    Ret
                ))

                List(
                    Call(printIntLabel)
                )
            case _ => List.empty
        }
    }

    implicit def typeToSize(declType: Type): Size = declType match {
        case ArrayType(_) => QWord
        case BoolType() => Byte
        case CharType() => Byte
        case IntType() => DWord
        case StringType() => QWord
        case ErasedPair() => QWord
        case PairType(_, _) => QWord
    }

    def translateRValue(rvalue: RValue, targetReg: Operand = Reg(Rax)): List[ASMItem] = rvalue match {
        case expr: Expr => translateExpression(expr, targetReg)
        case _ => List.empty
    }

    def addLabel(label: Label, asmList: List[ASMItem]) = 
        labelMap.addOne(label.ident, label :: asmList)
    
    def generateLabels: List[ASMItem] =
        labelMap.flatMap(_._2).toList

    def translateExpression(expr: Expr, targetReg: Operand = Reg(Rax)): List[ASMItem] = {
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