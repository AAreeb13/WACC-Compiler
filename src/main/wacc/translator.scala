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
    val labelMap: HashMap[Label, List[ASMItem]] = HashMap.empty

    val topLevelTable = symbolTables.head
    val functionTables = symbolTables.tail

    val readOnlyMap: HashMap[Label, List[ASMItem]] = HashMap.empty

    var stringCounter = 0
    var stackOffset = 0

    val asmList: List[ASMItem] = translateProgram(prog)

    def translateProgram(prog: Prog): List[ASMItem] = {
        val programHeader = List(
            Text,
            Label("main"),
            Push(Reg(Rbp)),
            Mov(Reg(Rsp), Reg(Rbp)),
        )

        // for all the variables we need to 
        // make stacks 
        val programBody = prog.stats.flatMap(translateStatement(_)(topLevelTable))

        val popVariablesInScope = topLevelTable.table.map(_ => Pop(Reg(Rax))).toList

        val programFooter = List(
            Mov(ImmVal(0), Reg(Rax)),
            Pop(Reg(Rbp)),
            Ret
        )

        List(Global) ::: 
        generateReadOnlys :::
        programHeader :::
        // this can be abstracted out
        //allocateStackVariables(topLevelTable) :::
        programBody :::
        popStackVariables(topLevelTable) :::
        // upto here
        programFooter :::
        generateLabels
    }

    // def allocateStackVariables(scopeTable: SymbolTable): List[ASMItem] = {
    //     // generate add instruction with the sizes
    //     //val allocationSize = 0
    //     var currentOffset = 0
    //     val allocatedVariables = scopeTable.table.toSeq.flatMap { case (ident, (sizeType, _, loc)) => {
    //         sizeType match {
    //             case SemChar =>

    //             case SemInt =>
    //                 Sub(ImmVal(4), )
    //             case SemBool => 1
    //             case SemString =>
    //             case _ =>
    //         }
    //         Sub(ImmVal())
    //         scopeTable.table(ident).get._3 =
    //     }}
    //     Sub(ImmVal(), Reg(Rsp))
    // }

    def popStackVariables(scopeTable: SymbolTable): List[ASMItem] = {
        asmIR.Add(ImmVal(scopeTable.currentScopeOffset), Reg(Rsp)) :: Nil
        // scopeTable.table.flatMap(_ => List(
        //     asmIR.Add(ImmVal(scopeTable.currentScopeOffset), Reg(Rsp))
        // )).toList
    }

    def translateStatement(stat: Stat)(implicit currentScope: SymbolTable): List[ASMItem] = {
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

            case Assign(lvalue, rvalue) =>
                translateRValue(rvalue, Reg(Rax)) :::
                translateLValue(lvalue, Reg(Rbx)) :::
                List(Mov(Reg(Rax), Reg(Rbx)))
                

            case AssignNew(declType, ident, rvalue) =>
                translateRValue(rvalue) :::
                nextStackLocation(ident, declType)
            
            case p@Print(expr) =>
                translateExpression(expr) :::
                List(Mov(Reg(Rax), Reg(Rdi))) :::
                translatePrint(p.enclosingType)
            
            case p@Println(expr) =>
                translateExpression(expr) :::
                List(Mov(Reg(Rax), Reg(Rdi))) :::
                translatePrint(p.enclosingType) :::
                translateNewline()

            case r@Read(lvalue) =>
                translateLValue(lvalue) :::
                List(Mov(Reg(Rax), Reg(Rdi))) :::
                translateRead(r) 

            case _ => List.empty
        }
    }

    def nextStackLocation(ident: String, declType: Type)(implicit currentScope: SymbolTable): List[ASMItem] = {
        val size = declType match {
            case BoolType() => 1
            case CharType() => 1
            case IntType() => 4
            case _ => 0
        }

        val memLocation: Option[Operand] = declType match {
            case BoolType() | CharType() | IntType() =>
                val retVal = Some(Mem(Reg(Rbp), ImmVal(stackOffset -  currentScope.currentScopeOffset)))
                currentScope.updateStackLocation(ident, retVal)
                retVal
            case _ => None
        }

        val retVal: List[ASMItem] = declType match {
            case BoolType() =>
                asmIR.Sub(ImmVal(size), Reg(Rsp)) ::
                Mov(Reg(Rax, Byte), memLocation.get, Byte) :: Nil
            case CharType() =>
                asmIR.Sub(ImmVal(size), Reg(Rsp)) ::
                Mov(Reg(Rax, Byte), memLocation.get, Byte) :: Nil
            case IntType() =>
                asmIR.Sub(ImmVal(size), Reg(Rsp)) ::
                Mov(Reg(Rax, DWord), memLocation.get, DWord) :: Nil
            case _ => Nil
        }

        stackOffset += size
        retVal
    }

    def translatePrint(innerType: SemType): List[ASMItem] = {
        innerType match {
            case SemInt => // SemInt and SemChar are repeated code, except for label names and format specifier -> factor common code
                val printLabel = Label("_printi")
                val printStringLabel = Label(".L._printi_str0")
                val mask = -16

                addReadOnly(printStringLabel, StringDecl("%d", printStringLabel))

                addLabel(printLabel, List(
                    Push(Reg(Rbp)),
                    Mov(Reg(Rsp), Reg(Rbp)),
                    asmIR.And(ImmVal(mask), Reg(Rsp)),
                    Mov(Reg(Rdi, DWord), Reg(Rsi, DWord), DWord),
                    Lea(Mem(Reg(Rip), printStringLabel), Reg(Rdi)),
                    Mov(ImmVal(0), Reg(Rax, Byte), Byte),
                    Call(LibFunc.Printf),
                    Mov(ImmVal(0), Reg(Rdi)),
                    Call(LibFunc.Flush),
                    Mov(Reg(Rbp), Reg(Rsp)),
                    Pop(Reg(Rbp)),
                    Ret
                ))

                List(
                    Call(printLabel)
                )
            case SemChar =>
                val printLabel = Label("_printc")
                val printStringLabel = Label(".L._printc_str0")
                val mask = -16

                addReadOnly(printStringLabel, StringDecl("%c", printStringLabel))

                addLabel(printLabel, List(
                    Push(Reg(Rbp)),
                    Mov(Reg(Rsp), Reg(Rbp)),
                    asmIR.And(ImmVal(mask), Reg(Rsp)),
                    Mov(Reg(Rdi, Byte), Reg(Rsi, Byte), Byte),
                    Lea(Mem(Reg(Rip), printStringLabel), Reg(Rdi)),
                    Mov(ImmVal(0), Reg(Rax, Byte), Byte),
                    Call(LibFunc.Printf),
                    Mov(ImmVal(0), Reg(Rdi)),
                    Call(LibFunc.Flush),
                    Mov(Reg(Rbp), Reg(Rsp)),
                    Pop(Reg(Rbp)),
                    Ret
                ))

                List(
                    Call(printLabel)
                )
            
            case SemString =>
                val printLabel = Label("_prints")
                val printStringLabel = Label(".L._prints_str0")
                val mask = -16

                addReadOnly(printStringLabel, StringDecl("%.*s", printStringLabel))

                addLabel(printLabel, List(
                    Push(Reg(Rbp)),
                    Mov(Reg(Rsp), Reg(Rbp)),
                    asmIR.And(ImmVal(mask), Reg(Rsp)),
                    Mov(Reg(Rdi), Reg(Rdx)),
                    Mov(Mem(Reg(Rdi), ImmVal(-4)), Reg(Rsi, DWord), DWord),
                    Lea(Mem(Reg(Rip), printStringLabel), Reg(Rdi)),
                    Mov(ImmVal(0), Reg(Rax, Byte), Byte),
                    Call(LibFunc.Printf),
                    Mov(ImmVal(0), Reg(Rdi)),
                    Call(LibFunc.Flush),
                    Mov(Reg(Rbp), Reg(Rsp)),
                    Pop(Reg(Rbp)),
                    Ret
                ))

                List(
                    Call(printLabel)
                )
            
            case SemBool =>
                val printLabel = Label("_printb")
                val printFalseLabel = Label(".L._printb_str0")
                val printTrueLabel = Label(".L._printb_str1")
                val printStringLabel = Label(".L._printb_str2")

                val falseLabel = Label(".L_printb0")
                val trueLabel = Label(".L_printb1")

                val mask = -16

                addReadOnly(printFalseLabel, StringDecl("false", printFalseLabel))
                addReadOnly(printTrueLabel, StringDecl("true", printTrueLabel))
                addReadOnly(printStringLabel, StringDecl("%.*s", printStringLabel))

                addLabel(printLabel, List(
                    Push(Reg(Rbp)),
                    Mov(Reg(Rsp), Reg(Rbp)),
                    asmIR.And(ImmVal(mask), Reg(Rsp)),
                    Cmp(ImmVal(0), Reg(Rdi, Byte), Byte),
                    Jmp(falseLabel, NotEqual),
                    Lea(Mem(Reg(Rip), printFalseLabel), Reg(Rdx)),
                    Jmp(trueLabel),
                    falseLabel,
                    Lea(Mem(Reg(Rip), printTrueLabel), Reg(Rdx)),
                    trueLabel,
                    Mov(Mem(Reg(Rdx), ImmVal(-4)), Reg(Rsi, DWord), DWord),
                    Lea(Mem(Reg(Rip), printStringLabel), Reg(Rdi)),
                    Mov(ImmVal(0), Reg(Rax, Byte), Byte),
                    Call(LibFunc.Printf),
                    Mov(ImmVal(0), Reg(Rdi)),
                    Call(LibFunc.Flush),
                    Mov(Reg(Rbp), Reg(Rsp)),
                    Pop(Reg(Rbp)),
                    Ret
                ))

                List(
                    Call(printLabel)
                )
            case _ => List.empty
        }
    }

    def translateNewline(): List[ASMItem] = {
        val printLabel = Label("_println")
        val printStringLabel = Label(".L._println_str0")
        val mask = -16

        addReadOnly(printStringLabel, StringDecl("", printStringLabel))

        addLabel(printLabel, List(
            Push(Reg(Rbp)),
            Mov(Reg(Rsp), Reg(Rbp)),
            asmIR.And(ImmVal(mask), Reg(Rsp)),
            Lea(Mem(Reg(Rip), printStringLabel), Reg(Rdi)),
            Call(LibFunc.Puts),
            Mov(ImmVal(0), Reg(Rdi)),
            Call(LibFunc.Flush),
            Mov(Reg(Rbp), Reg(Rsp)),
            Pop(Reg(Rbp)),
            Ret
        ))

        List(
            Call(printLabel)
        )
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

    def translateRValue(rvalue: RValue, targetReg: Reg = Reg(Rax))(implicit currentScope: SymbolTable): List[ASMItem] = rvalue match {
        case expr: Expr => translateExpression(expr, targetReg)
        case _ => List.empty
    }

    def addLabel(label: Label, asmList: =>List[ASMItem]) = {
        if (!labelMap.contains(label)) {
            labelMap.addOne(label, label :: asmList)
        }
    }
    
    def addReadOnly(label: Label, readOnly: =>StringDecl) = {
        if (!readOnlyMap.contains(label)) {
            readOnlyMap.addOne(label, readOnly :: Nil)
        }
    }

    def generateReadOnlys: List[ASMItem] =
        Readonly :: readOnlyMap.flatMap(_._2).toList
    
    def generateLabels: List[ASMItem] =
        labelMap.flatMap(_._2).toList

    def translateExpression(expr: Expr, targetReg: Reg = Reg(Rax))(implicit currentScope: SymbolTable): List[ASMItem] = {
        expr match {
            case IntVal(int) => 
                List(
                    Mov(ImmVal(int), targetReg)
                )
            case CharVal(char) => 
                List(
                    Mov(ImmVal(char.toInt), targetReg)
                )
            case BoolVal(bool) =>
                List(
                    Mov(ImmVal(if (bool) 1 else 0), targetReg)
                )
            case StrVal(str) => 
                val strLabel = Label(s".L.str${stringCounter}")
                addReadOnly(strLabel, StringDecl(str, strLabel))
                stringCounter += 1
                List(
                    Lea(Mem(Reg(Rip), strLabel), targetReg)
                )
            case variable: Var => translateVar(variable, targetReg)
            case expr: ArithmeticOp => 
                transArithmeticOp(expr, targetReg.toSize(DWord)) 
            case _ => List.empty
        }
    }

    def translateLValue(lvalue: LValue, targetReg: Reg = Reg(Rax))(implicit currentScope: SymbolTable): List[ASMItem] = {
        lvalue match {
            case variable: Var => translateVar(variable, targetReg)
            case _ => List.empty

        }
    }

    def transArithmeticOp(expr: Expr, targetReg: Reg = Reg(Rax, DWord))(implicit currentScope: SymbolTable): List[ASMItem] = {
        expr match {
            case IntVal(int) => Mov(ImmVal(int), targetReg, DWord) :: Nil
            case variable: Var => translateVar(variable, targetReg, DWord)
            case expr: ArithmeticOp =>
                List(
                    Push(Reg(R12))
                ) ::: 
                transArithmeticOp(expr.x, targetReg) ::: 
                transArithmeticOp(expr.y, Reg(R12, DWord)) ::: 
                binOp(expr, Reg(R12, DWord), targetReg) :::
                List(
                    Movs(targetReg, targetReg.toSize(QWord), DWord),
                    Pop(Reg(R12))
                )
            case _ => List.empty
        }
    }
       
    def binOp(op: BinOp, src: Reg, targetReg: Reg = Reg(Rax, DWord)): List[ASMItem] = {
        op match {
            case ast.Add(_, _) => asmIR.Add(src, targetReg, DWord) :: Nil
            case ast.Sub(_, _) => asmIR.Sub(src, targetReg, DWord) :: Nil 
            case _ => List.empty
        }
    }

    def translateVar(v: Var, targetReg: Reg = Reg(Rax), size: Size = QWord)(implicit currentScope: SymbolTable): List[ASMItem] = {
        val (declType, _, location) = currentScope.table.get(v.v).get
        declType match {
            case SemBool => Movs(location.get, targetReg, Byte) :: Nil
            case SemChar => Movs(location.get, targetReg, Byte) :: Nil
            case SemInt  => size == DWord match {
                case true => Mov(location.get, targetReg, DWord) :: Nil
                case _ => Movs(location.get, targetReg, DWord) :: Nil
            }
            case _ => Nil
        }
    }

    def updateLValue(lvalue: LValue, source: Reg = Reg(Rax))(implicit currentScope: SymbolTable): List[ASMItem] = {
        lvalue match {
            case v: Var => 
                 val (declType, _, location) = currentScope.table.get(v.v).get
                 declType match {
                    case SemBool => Mov(source, location.get, Byte) :: Nil
                    case SemChar => Mov(source, location.get, Byte) :: Nil
                    case SemInt => Mov(source, location.get, DWord) :: Nil
                    case _ => Nil
                 }               
            case _ => ???
        }
    }

    private def translateReadBuilder(readLabelIdent: String, escapeChar: String, size: Size): List[ASMItem] = {
        val readLabel = Label(readLabelIdent)
        val readStringLabel = Label(".L." + readLabelIdent + "_str0")
        val mask = -16
        
        addReadOnly(readStringLabel, StringDecl(escapeChar, readStringLabel))

        addLabel(readLabel, List(
                Push(Reg(Rbp)),
                Mov(Reg(Rsp), Reg(Rbp)),
                asmIR.And(ImmVal(mask), Reg(Rsp)),
                asmIR.Sub(ImmVal(16), Reg(Rsp)),
                Mov(Reg(Rdi, size), Mem(Reg(Rsp)), size),
                Lea(Mem(Reg(Rsp)), Reg(Rsi)),
                Lea(Mem(Reg(Rip), readStringLabel), Reg(Rdi)),
                Mov(ImmVal(0), Reg(Rax, Byte), Byte),
                Call(LibFunc.Scanf),
                Movs(Mem(Reg(Rsp)), Reg(Rax), size),
                asmIR.Add(ImmVal(16), Reg(Rsp)),
                Mov(Reg(Rbp), Reg(Rsp)),
                Pop(Reg(Rbp)),
                Ret
        ))

        List(
            Call(readLabel)
        )
    }

/* generates the read label and updates the variable read into with the value provided from stdin */
    def translateRead(r: Read)(implicit currentScope: SymbolTable): List[ASMItem] = r.enclosingType match {
        case SemInt => translateReadBuilder("_readi", "%d", DWord) ::: updateLValue(r.lvalue, Reg(Rax, DWord))
        case SemChar => translateReadBuilder("_readc", "%c", Byte) ::: updateLValue(r.lvalue, Reg(Rax, Byte))
        case _ => List.empty
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