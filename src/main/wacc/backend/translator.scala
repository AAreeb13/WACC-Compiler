package wacc

import wacc.ast._
import wacc.asm._
import wacc.asm.ComparisonType._
import wacc.asm.InstructionSize._
import wacc.asm.RegisterNames._
import wacc.Implicits.syntaxToSem

import scala.collection.mutable.ListBuffer
import scala.collection.mutable.HashMap
import scala.language.implicitConversions
import scala.collection.mutable.Stack

object codeGenerator {
    def translate(astInfo: (Node, List[SymbolTable])): Either[String, String] = translate(astInfo._1, astInfo._2)
    def translate(ast: Node, symbolTables: List[SymbolTable]): Either[String, String] = {
        ast match {
            case prog: Prog =>
                Right(new Translator(prog, symbolTables).toAssembly)
            case _ => Left("Invalid AST type for error generation")
        }
    }
}

class Translator(prog: Prog, val symbolTables: List[SymbolTable]) {
    val labelMap: HashMap[Label, List[ASMItem]] = HashMap.empty

    val readOnlyMap: HashMap[Label, List[ASMItem]] = HashMap.empty

    var stringCounter = 0
    var labelCounter = 0
    var stackOffset: ListBuffer[Int] = ListBuffer.empty

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
        stackOffset.addOne(0)
        val programBody = prog.stats.flatMap(translateStatement(_)(prog.scope))
        val functions = prog.funcs.flatMap(f => translateFunction(f)(f.scope))

        val programFooter = List(
            Mov(ImmVal(0), Reg(Rax)),
            Pop(Reg(Rbp)),
            Ret
        )

        List(Global) ::: 
        generateReadOnlys :::
        programHeader :::
        allocateStackVariables(prog.scope) :::
        programBody :::
        popStackVariables(prog.scope) :::
        programFooter :::
        functions :::
        generateLabels
    }

    def translateFunction(func: Func)(implicit currentScope: SymbolTable): List[ASMItem] = {
        val functionHeader = List(
            Label(s"wacc_${func.name}"),
            Push(Reg(Rbp)),
            Mov(Reg(Rsp), Reg(Rbp)),
        )

        val paramScope = func.scope
        val funcBodyScope = paramScope.children.head

        stackOffset.addOne(0)
        val assignParameters = func.params.reverse.flatMap { param =>
            Pop(Reg(Rax)) ::
            nextStackLocation(param.name, syntaxToSem(param.declType))(paramScope)
        }
        stackOffset.remove(stackOffset.size - 1)

        val functionFooter = List(
            Mov(Reg(Rbp), Reg(Rsp)),
            Pop(Reg(Rbp)),
            Ret
        )

        functionHeader :::
        allocateStackVariables(paramScope) :::
        List(Pop(Reg(Rbx))) :::
        assignParameters :::
        List(Push(Reg(Rbx))) :::
        allocateStackVariables(funcBodyScope) :::
        func.stats.flatMap(translateStatement(_)(funcBodyScope)) :::
        popStackVariables(funcBodyScope) :::
        popStackVariables(paramScope) :::
        functionFooter 
    }

    def allocateStackVariables(scopeTable: SymbolTable): List[ASMItem] =
        scopeTable.currentScopeOffset match {
            case 0 => Nil
            case offset => List(asm.Sub(ImmVal(offset), Reg(Rsp)))
    }

    def popStackVariables(scopeTable: SymbolTable): List[ASMItem] =
        scopeTable.currentScopeOffset match {
            case 0 => Nil
            case offset => List(asm.Add(ImmVal(offset), Reg(Rsp)))
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
                    asm.And(ImmVal(mask), Reg(Rsp)),
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
                updateLValue(lvalue, Reg(Rax))  

            case Return(expr) =>  
                translateExpression(expr, Reg(Rax)) ::: Nil

            case AssignNew(declType, ident, rvalue) =>
                translateRValue(rvalue) :::
                nextStackLocation(ident, syntaxToSem(declType))
            
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

            case Scope(stats) =>
                val childTable = currentScope.children(0)
                allocateStackVariables(childTable) :::
                stats.flatMap(translateStatement(_)(childTable)) :::
                popStackVariables(childTable)
            
            case If(cond, ifStats, elseStats) =>
                val ifChild = currentScope.children(0)
                val elseChild = currentScope.children(1)
                val ifBranch = Label(s".L$labelCounter")
                labelCounter += 1
                val endBranch = Label(s".L$labelCounter")
                labelCounter += 1

                translateExpression(cond) :::
                List(
                    Jmp(ifBranch, Equal)
                ) :::
                allocateStackVariables(elseChild) :::
                ifStats.flatMap(translateStatement(_)(elseChild)) :::
                popStackVariables(elseChild) :::
                List(
                    Jmp(endBranch),
                    ifBranch
                ) :::
                allocateStackVariables(ifChild) :::
                elseStats.flatMap(translateStatement(_)(ifChild)) :::
                popStackVariables(ifChild) :::
                List(endBranch)

            case While(cond, stats) =>
                val childScope = currentScope.children(0)
                val checkBranch = Label(s".L$labelCounter")
                labelCounter += 1
                val loopBranch = Label(s".L$labelCounter")
                labelCounter += 1

                Comment("Begin while") ::
                List(
                    Jmp(checkBranch),
                    loopBranch
                ) :::
                stats.flatMap(translateStatement(_)(childScope)) :::
                List(checkBranch) :::
                translateExpression(cond) :::
                List(
                    Cmp(ImmVal(1), Reg(Rax)),
                    Jmp(loopBranch, Equal)
                )

            case _ => List.empty
        }
    }

    def nextStackLocation(ident: String, size: Size)(implicit currentScope: SymbolTable): List[ASMItem] = {
        val memLocation: Option[Operand] = Some(Mem(Reg(Rbp), ImmVal(stackOffset.last -  currentScope.currentScopeOffset)))
        currentScope.updateStackLocation(ident, memLocation)
        stackOffset(stackOffset.size - 1) += size
        
        List(
            Comment(s"stackOffset: ${stackOffset.last}"),
            Mov(Reg(Rax, size), memLocation.get, size)
        )
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
                    asm.And(ImmVal(mask), Reg(Rsp)),
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
                    asm.And(ImmVal(mask), Reg(Rsp)),
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
                    asm.And(ImmVal(mask), Reg(Rsp)),
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
                    asm.And(ImmVal(mask), Reg(Rsp)),
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
            asm.And(ImmVal(mask), Reg(Rsp)),
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

    // implicit def typeToSize(declType: Type): Size = declType match {
    //     case ArrayType(_) => QWord
    //     case BoolType() => Byte
    //     case CharType() => Byte
    //     case IntType() => DWord
    //     case StringType() => QWord
    //     case ErasedPair() => QWord
    //     case PairType(_, _) => QWord
    // }

    implicit def typeToSize(declType: SemType): Size = declType match {
        case SemBool | SemChar => Byte
        case SemInt => DWord
        case _ => QWord
    }

    implicit def sizeToInt(size: Size): Int = size match {
        case QWord => 8
        case DWord => 4
        case Word => 2
        case Byte => 1
    }

    def translateRValue(rvalue: RValue, targetReg: Reg = Reg(Rax))(implicit currentScope: SymbolTable): List[ASMItem] = rvalue match {
        case expr: Expr => translateExpression(expr, targetReg)
        case f@FuncCall(ident, args) =>
            /*
            translate each expression then store the result into stack location (allocate new variable)
            then each time a parameter needs to be accessed we offset the size 
            */
            allocateStackVariables(f.func.scope) :::
            args.zip(f.paramTypes).zip(f.func.params.map(_.name)).flatMap{ case ((argExpr, argType), argIdent) => 
                translateExpression(argExpr) :::
                nextStackLocation(argIdent, argType)
            } :::
            List(
                Call(Label(s"wacc_$ident")),
                Mov(Reg(Rax), targetReg)
            ) :::
            popStackVariables(f.func.scope)

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

    def toRaw(s: String) =
        s.flatMap(_ match {
            case '\\' => "\\\\"
            case '\"' => "\\\""
            case '\'' => "\\\'"
            case '\n' => "\\n"
            case '\t' => "\\t"
            case '\f' => "\\f"
            case '\r' => "\\r"
            case '\b' => "\\b"
            case other => other.toString
        })

    def translateExpression(expr: Expr, targetReg: Reg = Reg(Rax), size: Size = QWord)(implicit currentScope: SymbolTable): List[ASMItem] = {
        expr match {
            case IntVal(int) => 
                List(
                    Mov(ImmVal(int), targetReg, size)
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
                addReadOnly(strLabel, StringDecl(toRaw(str), strLabel))
                stringCounter += 1
                List(
                    Lea(Mem(Reg(Rip), strLabel), targetReg)
                )
            case variable: Var => translateVar(variable, targetReg, size)
            case Chr(expr) => 
                val asciiCheck = -128
                val mask = -16
                val badCharLabel = Label("_errBadChar")
                val badCharStringLabel = Label(".L._errBadChar_str0")
                addReadOnly(badCharStringLabel, StringDecl("fatal error: int %d is not ascii character 0-127 \\n", badCharStringLabel))

                addLabel(badCharLabel, List(
                    asm.And(ImmVal(mask), Reg(Rsp)),
                    Lea(Mem(Reg(Rip), badCharStringLabel), Reg(Rdi)),
                    Mov(ImmVal(0), Reg(Rax, Byte), Byte),
                    Call(LibFunc.Printf),
                    Mov(ImmVal(0), Reg(Rdi)),
                    Call(LibFunc.Flush),
                    Mov(ImmVal(-1), Reg(Rdi, Byte), Byte),
                    Call(LibFunc.Exit)
                ))

                translateExpression(expr, targetReg) ::: List(
                    Test(ImmVal(asciiCheck), targetReg),
                    CMov(targetReg, Reg(Rsi), NotEqual),
                    Jmp(Label("_errBadChar"), NotEqual)
                )
            case Ord(expr) => translateExpression(expr, targetReg)
            case Neg(expr) => Push(Reg(R12)) ::
                translateExpression(expr, Reg(R12)) :::
                List(
                    Mov(ImmVal(0), targetReg.toSize(DWord), DWord),
                    asm.Sub(Reg(R12, DWord), targetReg.toSize(DWord), DWord),
                    Pop(Reg(R12))
                )
            case ast.Not(expr) => 
                val trueVal = 1
                translateExpression(expr, targetReg) :::
                List(
                    Cmp(ImmVal(trueVal), targetReg),
                    Set(Reg(Rax, Byte), NotEqual),
                    Movs(Reg(Rax, Byte), targetReg, Byte)
                )
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

    def translateBinExpression(expr1: Expr, expr2: Expr, targetReg: Reg = Reg(Rax), size: Size = QWord)
    (implicit currentScope: SymbolTable, body: List[ASMItem]): List[ASMItem] = {  
        Push(Reg(R12)) ::
        translateExpression(expr1, targetReg, size) :::
        List(Push(targetReg.toSize(QWord))) :::
        translateExpression(expr2, targetReg, size) :::
        List(
            Mov(targetReg.toSize(QWord), Reg(R12)),
            Pop(targetReg.toSize(QWord))):::
        body :::
        List(Pop(Reg(R12)))
    }

    def translateArithmeticOp(expr: Expr, targetReg: Reg = Reg(Rax, DWord))(implicit currentScope: SymbolTable): List[ASMItem] = {
        expr match {
            case expr: ArithmeticOp =>
                implicit val body: List[ASMItem] = translateBinOp(expr, Reg(R12, DWord), targetReg) ::: Movs(targetReg, targetReg.toSize(QWord), DWord) :: Nil
                translateBinExpression(expr.x, expr.y, targetReg, DWord)
            case _ => List.empty
        }

    }
       
    def translateBinOp(op: BinOp, src: Reg, targetReg: Reg = Reg(Rax, DWord)): List[ASMItem] = {
        op match {
            case ast.Add(_, _) =>
                addOverflowError() 
                asm.Add(src, targetReg, DWord) ::
                asm.Jmp(Label("_errOverflow"), ComparisonType.Overflow):: Nil
            case ast.Sub(_, _) => 
                addOverflowError()
                asm.Sub(src, targetReg, DWord) ::
                asm.Jmp(Label("_errOverflow"), ComparisonType.Overflow):: Nil 
            case ast.Mul(_, _) => 
                addOverflowError()
                asm.IMul(src, targetReg, DWord) ::
                asm.Jmp(Label("_errOverflow"), ComparisonType.Overflow):: Nil
            case ast.Div(_, _) => 
                addDivzeroError()
                asm.Cmp(ImmVal(0), src, DWord) ::
                asm.Jmp(Label("_errDivZero"), ComparisonType.Equal) ::
                asm.IDiv(src, DWord) ::
                asm.Mov(Reg(Rax, DWord), targetReg, DWord)::  Nil
            case ast.Mod(_, _) => 
                addDivzeroError()
                asm.Cmp(ImmVal(0), src, DWord) ::
                asm.Jmp(Label("_errDivZero"), ComparisonType.Equal) ::
                asm.IDiv(src, DWord) ::
                asm.Mov(Reg(Rdx, DWord), targetReg, DWord) :: Nil

            case _ => List.empty
        }
    }

    def addOverflowError(): Unit = {
        val overflowLabel = Label("_errOverflow")
        val printStringLabel = Label(".L._errOverflow_str0")
        val mask = -16

        addReadOnly(printStringLabel, 
        StringDecl("fatal error: integer overflow or underflow occurred\\n", printStringLabel))

        addErrorLabel(overflowLabel, printStringLabel)
    }

    def addDivzeroError(): Unit = {
        val divzeroLabel = Label("_errDivZero")
        val printStringLabel = Label(".L._errDivZero_str0")
        val mask = -16

        addReadOnly(printStringLabel, 
        StringDecl("fatal error: division or modulo by zero\\n", printStringLabel))

        addErrorLabel(divzeroLabel, printStringLabel)
    }

    def addErrorLabel(errorLabel: Label, stringLabel: Label, mask: Int = -16) : Unit = {
        addLabel(errorLabel, List(
        asm.And(ImmVal(mask), Reg(Rsp)),
        Lea(Mem(Reg(Rip), stringLabel), Reg(Rdi))) ::: 
        translatePrint(SemString) ::: 
        List(Mov(ImmVal(-1), Reg(Rdi, Byte), Byte),
        Call(LibFunc.Exit)))
    }

    def translateVar(v: Var, targetReg: Reg = Reg(Rax), size: Size = QWord)(implicit currentScope: SymbolTable): List[ASMItem] = {
        val (declType, _, locationOption) = currentScope.get(v.v).get
        val location = locationOption.getOrElse(currentScope.previousLocation(v.v).get)

        Comment(s"Var: ${v.v}") ::
        {
            declType match {
                case SemBool => Movs(location, targetReg, Byte) :: Nil
                case SemChar => Movs(location, targetReg, Byte) :: Nil
                case SemInt  => size match {
                    case DWord => Mov(location, targetReg, DWord) :: Nil
                    case _     => Movs(location, targetReg, DWord) :: Nil
                }
                case SemString => Mov(location, targetReg, QWord) :: Nil
                case _ => Nil
            }
        }
    }

    def updateLValue(lvalue: LValue, source: Reg = Reg(Rax))(implicit currentScope: SymbolTable): List[ASMItem] = {
        lvalue match {
            case v: Var => 
                 val (declType, _, location) = currentScope.get(v.v).get
                 declType match {
                    case SemBool | SemChar | SemInt => Mov(source, location.get, source.size) :: Nil
                    case _ => Nil
                 }               
            case _ => ???
        }
    }

    private def translateReadBuilder(readLabelIdent: String, escapeChar: String, size: Size): List[ASMItem] = {
        val readLabel = Label(readLabelIdent)
        val readStringLabel = Label(".L." + readLabelIdent + "_str0")
        val mask = -16
        val readOffset = 16
        
        addReadOnly(readStringLabel, StringDecl(escapeChar, readStringLabel))

        addLabel(readLabel, List(
                Push(Reg(Rbp)),
                Mov(Reg(Rsp), Reg(Rbp)),
                asm.And(ImmVal(mask), Reg(Rsp)),
                asm.Sub(ImmVal(readOffset), Reg(Rsp)),
                Mov(Reg(Rdi, size), Mem(Reg(Rsp)), size),
                Lea(Mem(Reg(Rsp)), Reg(Rsi)),
                Lea(Mem(Reg(Rip), readStringLabel), Reg(Rdi)),
                Mov(ImmVal(0), Reg(Rax, Byte), Byte),
                Call(LibFunc.Scanf),
                Movs(Mem(Reg(Rsp)), Reg(Rax), size),
                asm.Add(ImmVal(readOffset), Reg(Rsp)),
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