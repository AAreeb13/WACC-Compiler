package wacc

import semanticChecker.SemanticInfo
import IR._
import scala.collection.mutable.ListBuffer
import ast._
import scala.collection.mutable.HashMap
import implicits._
import semAst._
import scala.collection.mutable.HashSet
import scala.collection.mutable.Stack
import globals._


/*
 * This function takes in the AST, function and symbol table and generates the IR code
 * 
 * @param semanticInfo SemanticInfo contains the AST, function and symbol table
 * codeGenerator is only used when semantic check is successful, so SemanticInfo is always present
 * @param targetConfig TODO
 * @return The IR code
 */
 
class Translator(val semanticInfo: SemanticInfo, val targetConfig: TargetConfig) {
    import targetConfig._

    var stackOffsets: Stack[Int] = Stack(0)
    var branchCounter: Int = 0
    var asmList: ListBuffer[Line] = ListBuffer.empty
    var stringSet: HashSet[StringLabel] = HashSet.empty
    var funcMap: HashMap[FuncLabel, ListBuffer[Line]] = HashMap.empty

    // Main entry point for translating the whole code into IR
    def translate(): List[Line] = {
        // Generate Header

        translateProg(semanticInfo.ast)

        asmList += GlobalTag
        if (!stringSet.isEmpty) {
            asmList += ReadonlyTag
            asmList ++= stringSet
        }
        asmList += TextTag

        asmList ++= funcMap.toList.flatMap{ case (func, instrs) => func :: instrs.toList }

        asmList.toList
    }

    
    def translateProg(prog: Prog) = {
        // prog.funcs.foreach(f => funcMap.addOne((WaccFuncLabel(f.name), ListBuffer.empty)))
        // funcMap.addOne((MainLabel, translateMain(prog)))
        // prog.funcs.foreach(f => funcMap.update(WaccFuncLabel(f.name), translateFunction(f)))
        funcMap.addOne((MainLabel, translateMain(prog)))
        prog.funcs.foreach(f => funcMap.addOne((WaccFuncLabel(f.name), translateFunction(f))))
    }

    def translateMain(prog: Prog)(implicit buf: ListBuffer[Line] = ListBuffer.empty): ListBuffer[Line] = {
        // init variables
        // translate each variable

        buf += PushASM(BasePointer)
        buf += MovASM(StackPointer, BasePointer)

        translateBlock(prog.stats)(buf, prog.scope)

        buf += MovASM(DefaultExitCode, ReturnReg)
        buf += PopASM(BasePointer)
        buf += RetASM

        buf
    }

    def allocateStackVariables()(implicit buf: ListBuffer[Line], st: SymbolTable) = {
        if (st.getScopeSize() > 0) {
            buf += Comment(s"The current scope size is ${st.getScopeSize()}")
            buf += SubASM(Imm(st.getScopeSize()), StackPointer, StackPointer)
        }
    }

    def popStackVariables()(implicit buf: ListBuffer[Line], st: SymbolTable) = {
        if (st.getScopeSize() > 0) {
            buf += AddASM(Imm(st.getScopeSize()), StackPointer, StackPointer)
        }
    }

    def translateStatement(stat: Stat)(implicit buf: ListBuffer[Line], st: SymbolTable): Unit = {
        stat match {
            case node: If         => translateIf(node)
            case node: Assign     => translateAssign(node)
            case node: AssignNew  => translateDeclaration(node)
            case node: Read       => translateRead(node)
            case node: While      => translateWhile(node)
            case node: Print      => translatePrint(node)
            case node: Println    => translatePrintln(node)
            case Skip()           => 
            case node: Exit       => translateExit(node)
            case node: Scope      => translateBlock(node.stats)(buf, st.children(0))
            case node: Free       => translateFree(node)
            case node: Return     => translateReturn(node)
        }
    }

    def translateIf(node: If)(implicit buf: ListBuffer[Line], st: SymbolTable): Unit = {
        // Implement translation for If statement here
        val trueLabel = JumpLabel(s".L_if_true_$branchCounter")
        val endLabel = JumpLabel(s".L_if_end_$branchCounter")
        branchCounter += 1

        buf += Comment("Begin IF")

        translateExpression(node.cond)
        buf += CmpASM(TrueImm, ScratchRegs.head)
        buf += JmpASM(trueLabel, Equal)
        translateBlock(node.elseStat)(buf, st.children(1))
        buf += JmpASM(endLabel)
        buf += trueLabel
        translateBlock(node.ifStat)(buf, st.children(0))
        buf += endLabel

        buf += Comment("End IF")
        
    }

    def translateAssign(node: Assign)(implicit buf: ListBuffer[Line], st: SymbolTable): Unit = {
        // Implement translation for Assign statement here
        translateRValue(node.rvalue)
        translateLValue(node.lvalue, true)
    }

    def translateDeclaration(node: AssignNew)(implicit buf: ListBuffer[Line], st: SymbolTable): Unit = {
        // Implement translation for Declaration statement here
        translateRValue(node.rvalue)
        assignLocation(node)
    }

    def assignLocation(node: Declarable)(implicit buf: ListBuffer[Line], st: SymbolTable): Unit = {
        // Implement translation for Declaration statement here
        node match {
            case _: AssignNew => 
                val memLocation = RegisterImmediateOffset(BasePointer, stackOffsets.head - st.getScopeSize())

                val size = semanticToSize(syntaxToSemanticType(node.declType))

                st.updateLocation(node.name, memLocation)
                incrementStackOffset(sizeToInt(size)) // should this be a - or +?

                buf += MovASM(ScratchRegs.head, memLocation, size)
            
            case _: Param => 
                val size = semanticToSize(syntaxToSemanticType(node.declType))

                val localMemLocation = RegisterImmediateOffset(StackPointer, st.getScopeSize() - size - stackOffsets.head) // todo
                val funcMemLocation = RegisterImmediateOffset(BasePointer, 16 + st.getScopeSize() - stackOffsets.head - sizeToInt(size))

                st.updateLocation(node.name, funcMemLocation)
                
                incrementStackOffset(sizeToInt(size))
                buf += MovASM(ScratchRegs.head, localMemLocation, size)

        }
            

    }

    def incrementStackOffset(size: Int) = {
        assert(!stackOffsets.isEmpty, "Stack offsets should not be empty")
        stackOffsets.push(stackOffsets.pop() + size)
    }

    def translateRead(node: Read)(implicit buf: ListBuffer[Line], st: SymbolTable): Unit = {
        // Implement translation for Read statement here
        val (wrapperLabel, fstring) = (node.enclosingType: @unchecked) match {
            case SemChar => (ReadCharLabel, "%c")
            case SemInt => (ReadIntLabel, "%d")
        }

        if (!funcMap.contains(wrapperLabel)) {
            funcMap.addOne(wrapperLabel, translateReadLabel(wrapperLabel, fstring, semanticToSize(node.enclosingType)))
        }

        buf += CallASM(wrapperLabel)
    }

    def translateWhile(node: While)(implicit buf: ListBuffer[Line], st: SymbolTable): Unit = {
        // Implement translation for While statement here
        val doneLabel = JumpLabel(s".L_while_done_$branchCounter")
        val repeatLabel = JumpLabel(s".L_while_repeat_$branchCounter")
        branchCounter += 1

        buf += Comment("Begin WHILE")

        buf += JmpASM(doneLabel)
        buf += repeatLabel
        translateBlock(node.stats)(buf, st.children(0))
        buf += doneLabel
        translateExpression(node.cond)
        buf += CmpASM(TrueImm, ScratchRegs.head, Byte)
        buf += JmpASM(repeatLabel, Equal)

        buf += Comment("End WHILE")
    }

    def translatePrint(node: Printable)(implicit buf: ListBuffer[Line], st: SymbolTable): Unit = {
        // Implement translation for Print statement here
        translateExpression(node.expr)
        buf += MovASM(ScratchRegs.head, ParamRegs.head)
        val (wrapperLabel, fstring) = (node.enclosingType: @unchecked) match {
            case SemBool => (PrintBoolLabel, "%.*s")
            case SemInt => (PrintIntLabel, "%d")
            case SemChar => (PrintCharLabel, "%c")
            case SemString | SemArray(SemChar) => (PrintStrLabel, "%.*s")

            case other => (PrintPtrLabel, "%p")
        }

        if (!funcMap.contains(wrapperLabel)) {
            funcMap.addOne(wrapperLabel, translatePrintLabel(wrapperLabel, fstring, node.enclosingType))
        }

        buf += CallASM(wrapperLabel)

    }

    def translatePrintln(node: Println)(implicit buf: ListBuffer[Line], st: SymbolTable): Unit = {
        // Implement translation for Println statement here
        translatePrint(node)

        if (!funcMap.contains(PrintlnLabel)) {
            funcMap.addOne((PrintlnLabel, translatePrintlnLabel))
        }

        buf += CallASM(PrintlnLabel)
    }

    def translateExit(node: Exit)(implicit buf: ListBuffer[Line], st: SymbolTable): Unit = {
        // Implement translation for Exit statement here
        if (!funcMap.contains(ExitWrapperLabel)) {
            funcMap.addOne(ExitWrapperLabel, translateExitLabel)
        }

        translateExpression(node.expr)
        buf += MovASM(ReturnReg, ParamRegs.head)
        buf += CallASM(ExitWrapperLabel)
    }

    def translateBlock(stats: List[Stat])(implicit buf: ListBuffer[Line], st: SymbolTable): Unit = {
        allocateStackVariables()
        stats.foreach(translateStatement(_))
        popStackVariables()
    }

    def translateFree(node: Free)(implicit buf: ListBuffer[Line], st: SymbolTable): Unit = {
        // Implement translation for Free statement here
        val freeLabel = if (node.isArray) FreeArrayLabel else FreePairLabel
        
        if (!funcMap.contains(freeLabel)) {
                funcMap.addOne(freeLabel, translateFreeLabel(node.isArray))
        }

        translateExpression(node.expr)

        if (node.isArray) {
            buf += Comment(s"array pointers are shifted forward by ${sizeToInt(DWord)} bytes, so correct it back to original pointer before free")
            buf += SubASM(Imm(sizeToInt(DWord)), ScratchRegs.head, ScratchRegs.head)
        }

        buf += MovASM(ScratchRegs.head, ParamRegs.head)
        buf += CallASM(freeLabel)
    }

    def translateReturn(node: Return)(implicit buf: ListBuffer[Line], st: SymbolTable): Unit = {
        // Implement translation for Return statement here
        translateExpression(node.expr)
        //buf += MovASM(ScratchRegs, ReturnReg)
    }

    def translateExpression(expr: Expr)(implicit buf: ListBuffer[Line], st: SymbolTable): Unit = {
        expr match {
            case v: Var => translateVar(v)
            case CharVal(c) => buf += MovASM(Imm(c.toInt), ScratchRegs.head)
            case StrVal(s) =>
                val stringLabel = addStringConstant(s)
                buf += LeaASM(RegisterLabelOffset(InstructionPointer, stringLabel), ScratchRegs.head)
            case BoolVal(b) => buf += MovASM(Imm(if (b) 1 else 0), ScratchRegs.head)
            case PairVal() => buf += MovASM(NullImm, ScratchRegs.head)
            case v: ArrayVal => translateArrayElem(v)
            case IntVal(x) => buf += MovASM(Imm(x), ScratchRegs.head)
            case node: BinOp => translateBinOp(node)
            case node: UnOp => translateUnOp(node)
            
        }
    }

    def translateBinOp(binop: BinOp)(implicit buf: ListBuffer[Line], st: SymbolTable): Unit = {
        buf += Comment(s"Begin expression $binop")
        binop match {
            case op@ArithmeticOp(lhs, rhs) =>
                translateExpression(rhs)
                buf += PushASM(ScratchRegs.head) // also maybe specify dword/long size for push/pop
                translateExpression(lhs)
                buf += PopASM(ScratchRegs(1)) // could be wrong, need to save lhs value

                op match {
                    case divMod @ (Mod(_, _) | Div(_, _)) =>
                        if (!funcMap.contains(CheckDivZeroLabel)) {
                            funcMap.addOne((CheckDivZeroLabel, translateDivZeroLabel))
                        }

                        buf += CmpASM(Imm(0), ScratchRegs(1), DWord)
                        buf += JmpASM(CheckDivZeroLabel, Equal)
                        buf += DivASM(ScratchRegs.head, ScratchRegs.head, ScratchRegs(1)) // note that first 2 args are ignored for x86

                        (divMod: @unchecked) match {
                            case _: Div =>// buf += MovASM(ScratchRegs.head, ScratchRegs.head, DWord)
                            case _: Mod => buf += MovASM(R3, ScratchRegs.head, DWord) // todo: specify divMod register in config
                        }
                        buf += MovsASM(ScratchRegs.head, ScratchRegs.head, DWord)
                    case other =>
                        if (!funcMap.contains(CheckOverflowLabel)) {
                            funcMap.addOne((CheckOverflowLabel, translateOverflowLabel))
                        }

                        (other: @unchecked) match {
                            case _: Add => buf += AddASM(ScratchRegs(1), ScratchRegs.head, ScratchRegs.head, DWord)
                            case _: Sub => buf += SubASM(ScratchRegs(1), ScratchRegs.head, ScratchRegs.head, DWord) // check commutativity
                            case _: Mul => buf += MulASM(ScratchRegs(1), ScratchRegs.head, ScratchRegs.head, DWord)
                        }
                        buf += JmpASM(CheckOverflowLabel, Overflow)
                        buf += MovsASM(ScratchRegs.head, ScratchRegs.head, DWord)
                }

            case op@ComparativeOp(lhs, rhs) =>  // covers both equality and comparison operators
                translateExpression(lhs)
                buf += PushASM(ScratchRegs(0))
                translateExpression(rhs)
                buf += PopASM(ScratchRegs(1)) // could be wrong, need to save lhs value
                buf += CmpASM(ScratchRegs(0), ScratchRegs(1))
                val flag = op match {
                    case _: Eql => Equal
                    case _: NotEql => NotEqual
                    case _: GrtEql => GreaterEqual
                    case _: Grt => Greater
                    case _: LessEql => LessEqual
                    case _: Less => IR.Less
                }
                buf += SetASM(ScratchRegs(0), flag)
                buf += MovsASM(ScratchRegs(0), ScratchRegs(0), Byte)

            case op@LogicalOp(lhs, rhs) =>
                translateExpression(lhs)

                val endLabel = JumpLabel(s".L_logical_op_$branchCounter")
                branchCounter += 1
                
                buf += CmpASM(TrueImm, ScratchRegs.head)
                val flag = op match {
                    case _: And => NotEqual
                    case _: Or => Equal
                }
                buf += JmpASM(endLabel, flag)
                translateExpression(rhs)
                buf += CmpASM(TrueImm, ScratchRegs.head)
                buf += endLabel
                buf += SetASM(ScratchRegs.head, Equal)
                buf += MovsASM(ScratchRegs.head, ScratchRegs.head, Byte)
                
            case _ => println("This should never happen")
        }

        buf += Comment(s"End expression")
    }

    def translateUnOp(unop: UnOp)(implicit buf: ListBuffer[Line], st: SymbolTable): Unit = {
        unop match {
            case Len(expr) => 
                translateExpression(expr)
                buf += MovASM(ScratchRegs.head, ScratchRegs(1)) // can this be optimised out?
                buf += MovsASM(RegisterImmediateOffset(ScratchRegs(1), -sizeToInt(DWord)), ScratchRegs.head, DWord)

            case Not(expr) =>
                translateExpression(expr)
                buf += CmpASM(TrueImm, ScratchRegs.head) // idk if this one is necessary
                buf += SetASM(ScratchRegs.head, NotEqual, Byte)
                buf += MovsASM(ScratchRegs.head, ScratchRegs.head, Byte)
            case Ord(expr) => translateExpression(expr)
            case Chr(expr) =>
                translateExpression(expr)

                if (!funcMap.contains(CheckBadCharLabel)) {
                    funcMap.addOne((CheckBadCharLabel, translateBadCharLabel))
                }

                buf ++= ListBuffer(
                    TestASM(Imm(-128), ScratchRegs.head),
                    MovASM(ScratchRegs.head, ParamRegs(1), NotEqual, QWord),
                    JmpASM(CheckBadCharLabel, NotEqual)
                )
            case Neg(expr) =>
                translateExpression(expr)

                if (!funcMap.contains(CheckOverflowLabel)) {
                    funcMap.addOne((CheckOverflowLabel, translateOverflowLabel))
                }

                buf ++= ListBuffer(
                    MovsASM(ScratchRegs(0), ScratchRegs(0), DWord),
                    MovASM(ScratchRegs(0), ScratchRegs(1)), // next scratch reg
                    MovASM(Imm(0), ScratchRegs(0), DWord),
                    SubASM(ScratchRegs(1), ScratchRegs(0), ScratchRegs(0), DWord),
                    JmpASM(CheckOverflowLabel, Overflow),
                    MovsASM(ScratchRegs(0), ScratchRegs(0), DWord),
                )
        }
    }

    def translateLValue(lvalue: LValue, writeTo: Boolean = false)(implicit buf: ListBuffer[Line], st: SymbolTable): Unit = {
        // Implement translation for LValues here
        lvalue match {
            case node: ArrayVal => translateArrayElem(node, writeTo)
            case node: PairElem => translatePairElem(node, writeTo)
            case node: Var => translateVar(node, writeTo)
        }
    }

    def translateRValue(rvalue: RValue)(implicit buf: ListBuffer[Line], st: SymbolTable): Unit = {
        // Implement translation for Return statement here
        rvalue match {
            case node: ArrayLiteral => translateArrayLiteral(node)
            case node: PairElem => translatePairElem(node)
            case node: PairCons => translatePairCons(node)
            case node: FuncCall => translateFuncCall(node)
            case node: Expr => translateExpression(node)
        }
    }

    def translateArrayLiteral(node: ArrayLiteral)(implicit buf: ListBuffer[Line], st: SymbolTable): Unit = {
        // Implement translation for Array Literals here
        if (!funcMap.contains(MallocWrapperLabel)) {
            funcMap.addOne((MallocWrapperLabel, translateMallocLabel))
        }

        val singleSize = semanticToSize(node.enclosingType)
        val totalSize = sizeToInt(DWord) + singleSize * node.exprs.size
        buf += MovASM(Imm(totalSize), ParamRegs.head, DWord)
        buf += CallASM(MallocWrapperLabel)
        buf += MovASM(ScratchRegs(0), ArrayPointer)
        buf += AddASM(Imm(sizeToInt(DWord)), ArrayPointer, ArrayPointer)
        buf += MovASM(Imm(node.exprs.size), ScratchRegs(0))
        buf += MovASM(ScratchRegs(0), RegisterImmediateOffset(ArrayPointer, -sizeToInt(DWord)))
        node.exprs.zipWithIndex.foreach { case (expr, index) =>
            translateExpression(expr)
            buf += MovASM(ScratchRegs(0), RegisterImmediateOffset(ArrayPointer, index * singleSize))
        }
        buf += MovASM(ArrayPointer, ScratchRegs(0))
    }

    def translatePairCons(node: PairCons)(implicit buf: ListBuffer[Line], st: SymbolTable): Unit = {
        // Implement translation for Free statement here
        if (!funcMap.contains(MallocWrapperLabel)) {
            funcMap.addOne((MallocWrapperLabel, translateMallocLabel))
        }

        buf += Comment(s"Begin Pair Cons $node")

        val PairSize = Imm(16)
        buf += MovASM(PairSize, ParamRegs.head, DWord)
        buf += CallASM(MallocWrapperLabel)
        buf += MovASM(ScratchRegs.head, ArrayPointer)
        translateExpression(node.fst)
        buf += MovASM(ScratchRegs.head, RegisterOffset(ArrayPointer))
        translateExpression(node.snd)
        buf += MovASM(ScratchRegs.head, RegisterImmediateOffset(ArrayPointer, sizeToInt(QWord)))
        buf += MovASM(ArrayPointer, ScratchRegs.head)

        buf += Comment("End Pair Cons")
        
    }
    
    def translateFuncCall(node: FuncCall)(implicit buf: ListBuffer[Line], st: SymbolTable): Unit = {
        // Implement translation for FuncCall statement here
        allocateStackVariables()(buf, node.funcInfo.func.scope)
        stackOffsets.push(0)
        node.args.zip(node.funcInfo.func.params).foreach { case (expr, param) =>
            translateExpression(expr)
            assignLocation(param)(buf, node.funcInfo.func.scope)
        }
        buf += CallASM(WaccFuncLabel(node.ident))
        stackOffsets.pop()
        popStackVariables()(buf, node.funcInfo.func.scope)
    }

    def translateVar(node: Var, writeTo: Boolean = false)(implicit buf: ListBuffer[Line], st: SymbolTable): Unit = {
        assert(st.contains(node.v), s"Variable ${node.v} was not contained in the symbol table")
        assert(st.hasLocation(node.v), s"The location of variable ${node.v} was not contained in the symbol table")

        val location = st.getLocation(node.v).get
        val declType = st.typeof(node.v).get
        val size = semanticToSize(declType)

        if (writeTo) {
            buf += MovASM(ScratchRegs.head, location, size)
        } else {
            size match {
                case QWord => buf += MovASM(location, ScratchRegs.head, size)
                case size => buf += MovsASM(location, ScratchRegs.head, size)
            }
        }

    }

    def translateArrayElem(node: ArrayVal, writeTo: Boolean = false)(implicit buf: ListBuffer[Line], st: SymbolTable): Unit = {
        /*
        Need to unwrap array one by one
        Only call store on the last label
        Store result of array load in array pointer (Rax -> R9)
        Get array location
        */

        var location = st.getLocation(node.v).get
        var innerSize = semanticToSize(node.enclosingType)

        if (writeTo) buf += PushASM(ScratchRegs.head)

        node.exprs.zipWithIndex.foreach { case (expr, index) =>
            val store = writeTo && index == node.exprs.size - 1
            val size = if (index == node.exprs.size - 1) innerSize else QWord
            val label = if (store) ArrayStoreLabel(size) else ArrayLoadLabel(size)

            if (!funcMap.contains(label)) {
                funcMap.addOne((label, translateArrayElemLabel(size, store)))
            }

            translateExpression(expr)
            buf += MovASM(ScratchRegs.head, IndexPointer, DWord)


            if (index != 0) buf += PopASM(location)

            buf += MovASM(location, ArrayPointer)

            if (store) buf += PopASM(ScratchRegs.head)

            buf += CallASM(label)

            if (index != node.exprs.size - 1) {
                location = ScratchRegs.head
                buf += PushASM(location)
            }
            
        }

    }

    /*
    /**
  * Retrieves the address of a pair element, performing null checks and potentially dereferencing the pointer.
  * Supports accessing the first or second element of a pair variable or an expression.
  * @param x The pair element (first or second) to be accessed.
  * @param st The symbol table for current scope variable and type information.
  * @param target The list of assembly instructions being generated.
  * @param dereference Indicates whether the address should be dereferenced to obtain the value.
  * @return The type of the element accessed, or ErrorType in case of an array element access attempt.
  */
  def getPairElemAddr(x: PairElem, st: symbolTable, target: ListBuffer[ASMInstr], dereference: Boolean): Type = {
    x match {
        case FstPairElem(f) => f match {
          case PairLValue(p)   => {
            val t = getPairElemAddr(p, st, target, dereference)
            target += Mov(RETURN_REG, MemAddr(RETURN_REG), 8)
            t
          }
          case VarLValue(v)    => {
            val (PairType(t, _), _, Some(mem)) = st.lookupAllIR(v.str).get
            target += Mov(SCRATCH_REG_1, mem, 8)
            target += Cmp(SCRATCH_REG_1, ImmVal(0), 8)
            target += Jump(Comp.Eq, LabelCall("_nullNaughtiness"))
            addNullDereferenceLabel()
            if (dereference) {
              target += Mov(RETURN_REG, MemAddr(SCRATCH_REG_1), 8)
            } else {
              target += Mov(RETURN_REG, SCRATCH_REG_1, 8)
            }
            t
          }
          case ArrLValue(ArrElem(v, ls)) => ErrorType.default
        }


      case SndPairElem(f) => f match {
          case PairLValue(p)   => {
            val t = getPairElemAddr(p, st, target, dereference)
            target += AddInstr(RETURN_REG, ImmVal(8), 8)
            target += Mov(RETURN_REG, MemAddr(RETURN_REG), 8)
            t
          }
          case VarLValue(v)    => {
            val (PairType(_, t), _, Some(mem)) = st.lookupAllIR(v.str).get
            target += Mov(SCRATCH_REG_1, mem, 8)
            target += Cmp(SCRATCH_REG_1, ImmVal(0), 8)
            target += Jump(Comp.Eq, LabelCall("_nullNaughtiness"))
            addNullDereferenceLabel()
            if (dereference) {
              target += Mov(RETURN_REG, MemAddr(SCRATCH_REG_1, 8), 8)
            } else {
              target += Mov(RETURN_REG, SCRATCH_REG_1, 8)
              target += AddInstr(RETURN_REG, ImmVal(8), 8)
            }
            t
          }
          case ArrLValue(ArrElem(v, ls)) => ErrorType.default
        }
    }
  }
    */

    def getPairAddress(lvalue: LValue)(implicit buf: ListBuffer[Line], st: SymbolTable): Unit = {
        lvalue match {
            case Var(name) => 
                buf += MovASM(st.getLocation(name).get, ScratchRegs.head)
            case node@ArrayVal(name, exprs)  =>
                buf += MovASM(st.getLocation(name).get, ScratchRegs.head)   // memory location of array saved to RAX
                exprs.zipWithIndex.foreach { case (expr, index) =>
                    val size = if (index == exprs.size - 1) semanticToSize(node.enclosingType) else QWord
                    val loadLabel = ArrayLoadLabel(size)

                    if (!funcMap.contains(loadLabel)) {
                        funcMap.addOne((loadLabel, translateArrayElemLabel(size, false)))
                    }

                    buf += PushASM(ScratchRegs.head) // save previous location
                    translateExpression(expr)        // translate expression
                    buf += MovASM(ScratchRegs.head, IndexPointer, DWord) // move result of expression to index pointer

                    buf += PopASM(ArrayPointer)
                    // buf += PopASM(ScratchRegs.head)
                    // buf += MovASM(RegisterOffset(ScratchRegs.head), ArrayPointer)      // save previous location to array pointer
                    buf += CallASM(loadLabel)
                }
            case p@PairElem(lvalue) =>
                getPairAddress(lvalue)

                if (!funcMap.contains(CheckNullLabel)) {
                    funcMap.addOne((CheckNullLabel, translateNullLabel))
                }

                buf += CmpASM(NullImm, ScratchRegs.head)
                buf += JmpASM(CheckNullLabel, Equal)
                
                p match {
                    // dereference the inner pointer to get the inner lvalue
                    case _: FstPair => buf += MovASM(RegisterOffset(ScratchRegs.head), ScratchRegs.head)
                    case _: SndPair => buf += MovASM(RegisterImmediateOffset(ScratchRegs.head, 8), ScratchRegs.head)
                }
            case _ =>
        }
    }

    // fst x[2]
    // the heap address is stored in rax
    /*
    arrLoad loads the value of the operand and stores the value held in Rax
    we want it to return a pointer to the location of the array value

    pre: 
    we know that this is only called by pairs. Everything is a pointer.
    */
    def getLValueAddress(lvalue: LValue)(implicit buf: ListBuffer[Line], st: SymbolTable): Unit = {
        (lvalue: @unchecked) match {
            case node@ArrayVal(name, exprs) => 
                buf += LeaASM(st.getLocation(name).get, ScratchRegs.head)   // memory location of array saved to RAX
                exprs.zipWithIndex.foreach { case (expr, index) =>
                    val size = if (index == exprs.size - 1) semanticToSize(node.enclosingType) else QWord
                    val loadLabel = ArrayLoadLabel(size)

                    if (!funcMap.contains(loadLabel)) {
                        funcMap.addOne((loadLabel, translateArrayElemLabel(size, false)))
                    }

                    buf += PushASM(ScratchRegs.head) // save previous location
                    translateExpression(expr)        // translate expression
                    buf += MovASM(ScratchRegs.head, IndexPointer, DWord) // move result of expression to index pointer

                    buf += PopASM(ArrayPointer)
                    // buf += PopASM(ScratchRegs.head)
                    // buf += MovASM(RegisterOffset(ScratchRegs.head), ArrayPointer)      // save previous location to array pointer
                    buf += CallASM(loadLabel)
                }

/*
leaq <mem> <reg>
the value of reg is memory address itself
dereference reg to get the value of what is held in mem
and also to adjust its contents
*/
            case p@PairElem(lvalue) =>
                getLValueAddress(lvalue) // address of the inner LValue is stored in RAX
                buf += MovASM(RegisterOffset(ScratchRegs.head), ScratchRegs.head)

                // doesn't hurt to put here, although currently only called by translatePairElem which adds this anyway by default
                if (!funcMap.contains(CheckNullLabel)) {
                    funcMap.addOne((CheckNullLabel, translateNullLabel))
                }

                buf += CmpASM(NullImm, ScratchRegs.head)
                buf += JmpASM(CheckNullLabel, Equal)
                
                p match {
                    // dereference the inner pointer to get the inner lvalue
                    case _: FstPair => 
                    case _: SndPair => buf += AddASM(Imm(sizeToInt(QWord)), ScratchRegs.head, ScratchRegs.head)
                }

            case Var(name) =>
                // -4(%rbp) -> %rax
                buf += LeaASM(st.getLocation(name).get, ScratchRegs.head)
        }

    }

    // nested pair: fst fst p
    def translatePairElem(node: PairElem, writeTo: Boolean = false)(implicit buf: ListBuffer[Line], st: SymbolTable): Unit = {
        // only 1D case, not nested yet
        /*
        If is not a variable, then the location is stored on the heap.
        We need a way to find what the location is for the inner pair elem

        So far translating anything doesn't return anything, it 
        */

        if (!funcMap.contains(CheckNullLabel)) {
            funcMap.addOne((CheckNullLabel, translateNullLabel))
        }

        // Rax contains (heap) address
        // Rbx contains the value to overwrite with
        
        val memLocation = RegisterOffset(ScratchRegs(0))
        val (src, dst) = if (writeTo) (ScratchRegs(1), memLocation)
                         else         (memLocation, ScratchRegs(0))

        if (writeTo) buf += PushASM(ScratchRegs.head)
        getPairAddress(node)
        if (writeTo) buf += PopASM(ScratchRegs(1))

        buf += Comment(s"Begin Pair Elem $node")

        // buf += CmpASM(NullImm, memLocation)
        // buf += JmpASM(CheckNullLabel, Equal)
        buf += MovASM(src, dst)

        buf += Comment(s"End Pair Elem")
    }

    def translatePairElem2(node: PairElem, writeTo: Boolean = false)(implicit buf: ListBuffer[Line], st: SymbolTable): Unit = {
        if (!funcMap.contains(CheckNullLabel)) {
            funcMap.addOne((CheckNullLabel, translateNullLabel))
        }
        
        val memLocation = ScratchRegs(0)
        val (src, dst) = if (writeTo) (ScratchRegs(1), memLocation)
                         else         (memLocation, ScratchRegs(0))

        if (writeTo) buf += PushASM(ScratchRegs.head)
        getPairAddress(node)
        if (writeTo) buf += PopASM(ScratchRegs(1))

        buf += MovASM(src, dst)
    }
    


    def translateFunction(func: Func)(implicit buf: ListBuffer[Line] = ListBuffer.empty): ListBuffer[Line] = {
        // init variables
        // translate each variable

        buf += PushASM(BasePointer)
        buf += MovASM(StackPointer, BasePointer)

        translateBlock(func.stats)(buf, func.scope)
        //func.stats.foreach(translateStatement(_))

        buf += MovASM(BasePointer, StackPointer)
        buf += PopASM(BasePointer)
        buf += RetASM

        buf
    }





    ///////////////// UTILITY FUNCTIONS ///////////////////

    def addStringConstant(string: String): StringLabel = {
        val retVal = StringLabel(s".L.str${stringSet.size}", toRaw(string))
        if (!stringSet.contains(retVal)) stringSet.add(retVal)
        retVal
    }

    def toRaw(s: String): String =
        s.flatMap(_ match {
            case '\u0000' => "\\\u0000"
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


    def translateFreeLabel(isArray: Boolean): ListBuffer[Line] = {
        var buf: ListBuffer[Line] = ListBuffer(
            PushASM(BasePointer),
            MovASM(StackPointer, BasePointer),
            AndASM(AlignmentMaskImm, StackPointer, StackPointer)
        )
        
        if (!isArray) {
            if (!funcMap.contains(CheckNullLabel)) {
                funcMap.addOne((CheckNullLabel, translateNullLabel))
            }

            buf += CmpASM(NullImm, ParamRegs.head)
            buf += JmpASM(CheckNullLabel, Equal)
        }
        
        buf ++= ListBuffer(
            CallASM(FreeLabel),
            MovASM(BasePointer, StackPointer),
            PopASM(BasePointer),
            RetASM
        )

        buf
    }
    
    def translateArrayElemLabel(size: Size, store: Boolean): ListBuffer[Line] = {
        if (!funcMap.contains(CheckBoundsLabel)) {
            funcMap.addOne((CheckBoundsLabel, translateBoundsLabel))
        }

        val (src, dst) = if (store) (ScratchRegs.head, RegisterMultiplierOffset(ArrayPointer, IndexPointer, size))
                         else (RegisterMultiplierOffset(ArrayPointer, IndexPointer, size), ScratchRegs.head)

        ListBuffer(
            Comment(s"Special calling convention: array ptr passed in $ArrayPointer, index in $IndexPointer, ${if (store) "value to store in" else "and return into"} ${ScratchRegs.head}"),
            PushASM(ScratchRegs(1)),
            CmpASM(Imm(0), IndexPointer, DWord),
            MovASM(IndexPointer, ParamRegs(1), IR.Less),
            JmpASM(CheckBoundsLabel, IR.Less),
            MovASM(RegisterImmediateOffset(ArrayPointer, -sizeToInt(DWord)), ScratchRegs(1), DWord),
            CmpASM(ScratchRegs(1), IndexPointer, DWord),
            MovASM(IndexPointer, ParamRegs(1), GreaterEqual),
            JmpASM(CheckBoundsLabel, GreaterEqual),
            if (store || size == QWord) MovASM(src, dst, size)
            else MovsASM(src, dst, size),
            PopASM(ScratchRegs(1)),
            RetASM
        )
    }

    def translateMallocLabel: ListBuffer[Line] = {
        if (!funcMap.contains(CheckOOMLabel)) {
            funcMap.addOne((CheckOOMLabel, translateOOMLabel))
        }

        ListBuffer(
            PushASM(BasePointer),
            MovASM(StackPointer, BasePointer),
            AndASM(AlignmentMaskImm, StackPointer, StackPointer),
            CallASM(MallocLabel),
            CmpASM(NullImm, ReturnReg),
            JmpASM(CheckOOMLabel, Equal),
            MovASM(BasePointer, StackPointer),
            PopASM(BasePointer),
            RetASM
        )
    }

    def translateExitLabel: ListBuffer[Line] = {
        ListBuffer(
            PushASM(BasePointer),
            MovASM(StackPointer, BasePointer),
            AndASM(AlignmentMaskImm, StackPointer, StackPointer),
            CallASM(ExitLabel),
            MovASM(BasePointer, StackPointer),
            PopASM(BasePointer),
            RetASM
        )
    }

    def translateReadLabel(label: WrapperFuncLabel, fstring: String, size: Size): ListBuffer[Line] = {
        val stringLabel = StringLabel(s".L.${label.name}_string", fstring)

        stringSet.addOne(stringLabel)

        ListBuffer(
            PushASM(BasePointer),
            MovASM(StackPointer, BasePointer),
            AndASM(AlignmentMaskImm, StackPointer, StackPointer),
            SubASM(ReadOffsetImm, StackPointer, StackPointer),
            MovASM(ParamRegs(0), RegisterOffset(StackPointer), size),
            LeaASM(RegisterOffset(StackPointer), ParamRegs(1)),
            LeaASM(RegisterLabelOffset(InstructionPointer, stringLabel), ParamRegs(0)),
            MovASM(Imm(0), R0, Byte), // for SIMD purposes
            CallASM(ScanFormatted),
            MovsASM(RegisterOffset(StackPointer), ReturnReg, size),
            AddASM(ReadOffsetImm, StackPointer, StackPointer),
            MovASM(BasePointer, StackPointer),
            PopASM(BasePointer),
            RetASM
        )
    }

    def translatePrintLabel(label: WrapperFuncLabel, fstring: String, _type: SemType): ListBuffer[Line] = {
        val stringLabel = StringLabel(s".L.${label.name}_string", fstring)
        val size = semanticToSize(_type)

        stringSet.addOne(stringLabel)

        val buf: ListBuffer[Line] = ListBuffer(
            PushASM(BasePointer),
            MovASM(StackPointer, BasePointer),
            AndASM(AlignmentMaskImm, StackPointer, StackPointer)
        )

        _type match {
            case SemBool =>
                val trueLabel = JumpLabel(s".L${label.name}_true")
                val falseLabel = JumpLabel(s".L${label.name}_false")
                val trueStringLabel = StringLabel(s".L.${label.name}_string_true", "true")
                val falseStringLabel = StringLabel(s".L.${label.name}_string_false", "false")

                stringSet.addOne(trueStringLabel)
                stringSet.addOne(falseStringLabel)

                buf ++= ListBuffer(
                    CmpASM(FalseImm, ParamRegs.head),
                    JmpASM(falseLabel, NotEqual),
                    LeaASM(RegisterLabelOffset(InstructionPointer, falseStringLabel), ParamRegs(2)),
                    JmpASM(trueLabel),
                    falseLabel,
                    LeaASM(RegisterLabelOffset(InstructionPointer, trueStringLabel), ParamRegs(2)),
                    trueLabel,
                    MovASM(RegisterImmediateOffset(ParamRegs(2), -4), ParamRegs(1), DWord)
                )
            case SemString | SemArray(SemChar) =>
                buf += MovASM(ParamRegs(0), ParamRegs(2))
                buf += MovASM(RegisterImmediateOffset(ParamRegs(0), -4), ParamRegs(1), DWord)
            case other =>
                buf += MovASM(ParamRegs(0), ParamRegs(1), size)
        }

        buf ++= ListBuffer(
            LeaASM(RegisterLabelOffset(InstructionPointer, stringLabel), ParamRegs.head),
            MovASM(Imm(0), R0, Byte), // for SIMD purposes
            CallASM(PrintFormatted),
            MovASM(Imm(0), ParamRegs.head),
            CallASM(FileFlush),
            MovASM(BasePointer, StackPointer),
            PopASM(BasePointer),
            RetASM
        )

        buf
    }

    def translatePrintlnLabel: ListBuffer[Line] = {
        val stringLabel = StringLabel(s".L.println_string", "")
        stringSet.addOne(stringLabel)

        ListBuffer(
            PushASM(BasePointer),
            MovASM(StackPointer, BasePointer),
            AndASM(AlignmentMaskImm, StackPointer, StackPointer),
            LeaASM(RegisterLabelOffset(InstructionPointer, stringLabel), ParamRegs.head),
            CallASM(Puts),
            MovASM(Imm(0), ParamRegs.head),
            CallASM(FileFlush),
            MovASM(BasePointer, StackPointer),
            PopASM(BasePointer),
            RetASM
        )
    }

    ////////////////// ERRORS ////////////////////

    def translateNullLabel: ListBuffer[Line] = {
        val errorLabel = StringLabel(s".L._errNull_string", if (passTests) "#runtime_error#" else "fatal error: null pair dereferenced or freed\\n")
        stringSet.addOne(errorLabel)

        if (!funcMap.contains(PrintStrLabel)) {
            funcMap.addOne((PrintStrLabel, translatePrintLabel(PrintStrLabel, "%.*s", SemString)))
        }

        ListBuffer(
            AndASM(AlignmentMaskImm, StackPointer, StackPointer),
            LeaASM(RegisterLabelOffset(InstructionPointer, errorLabel), ParamRegs.head),
            CallASM(PrintStrLabel),
            MovASM(Imm(-1), ParamRegs.head, Byte),
            CallASM(ExitLabel)
        )
    }

    def translateBoundsLabel: ListBuffer[Line] = {
        val errorLabel = StringLabel(s".L._errOutOfBounds_string", if (passTests) "#runtime_error#" else "fatal error: array index %d out of bounds\\n")
        stringSet.addOne(errorLabel)

        ListBuffer(
            AndASM(AlignmentMaskImm, StackPointer, StackPointer),
            LeaASM(RegisterLabelOffset(InstructionPointer, errorLabel), ParamRegs.head),
            MovASM(Imm(0), R0, Byte),
            CallASM(PrintFormatted),
            MovASM(Imm(0), ParamRegs.head),
            CallASM(FileFlush),
            MovASM(Imm(-1), ParamRegs.head, Byte),
            CallASM(ExitLabel)
        )
    }
    
    def translateOOMLabel: ListBuffer[Line] = {
        val errorLabel = StringLabel(s".L._errOutOfMemory_string", if (passTests) "#runtime_error#" else "fatal error: out of memory\\n")
        stringSet.addOne(errorLabel)

        if (!funcMap.contains(PrintStrLabel)) {
            funcMap.addOne((PrintStrLabel, translatePrintLabel(PrintStrLabel, "%.*s", SemString)))
        }

        ListBuffer(
            AndASM(AlignmentMaskImm, StackPointer, StackPointer),
            LeaASM(RegisterLabelOffset(InstructionPointer, errorLabel), ParamRegs.head),
            CallASM(PrintStrLabel),
            MovASM(Imm(-1), ParamRegs.head, Byte),
            CallASM(ExitLabel)
        )
    }

    def translateOverflowLabel: ListBuffer[Line] = {
        val errorLabel = StringLabel(s".L._errOverflow_string", if (passTests) "#runtime_error#" else "fatal error: integer overflow or underflow occurred\\n")
        stringSet.addOne(errorLabel)

        if (!funcMap.contains(PrintStrLabel)) {
            funcMap.addOne((PrintStrLabel, translatePrintLabel(PrintStrLabel, "%.*s", SemString)))
        }

        ListBuffer(
            AndASM(AlignmentMaskImm, StackPointer, StackPointer),
            LeaASM(RegisterLabelOffset(InstructionPointer, errorLabel), ParamRegs.head),
            CallASM(PrintStrLabel),
            MovASM(Imm(-1), ParamRegs.head, Byte),
            CallASM(ExitLabel)
        )
    }


    def translateDivZeroLabel: ListBuffer[Line] = { // identical to translateOverflow - maybe simplify
        val errorLabel = StringLabel(s".L._errDivZero_string", if (passTests) "#runtime_error#" else "fatal error: division or modulo by zero\\n")
        stringSet.addOne(errorLabel)

        if (!funcMap.contains(PrintStrLabel)) {
            funcMap.addOne((PrintStrLabel, translatePrintLabel(PrintStrLabel, "%.*s", SemString)))
        }

        ListBuffer(
            AndASM(AlignmentMaskImm, StackPointer, StackPointer),
            LeaASM(RegisterLabelOffset(InstructionPointer, errorLabel), ParamRegs.head),
            CallASM(PrintStrLabel),
            MovASM(Imm(-1), ParamRegs.head, Byte),
            CallASM(ExitLabel)
        )
    }

    def translateBadCharLabel: ListBuffer[Line] = {
        val errorLabel = StringLabel(s".L._errBadChar_string", if (passTests) "#runtime_error#" else "fatal error: int %d is not ascii character 0-127\\n")
        stringSet.addOne(errorLabel)

        ListBuffer(
            AndASM(AlignmentMaskImm, StackPointer, StackPointer),
            LeaASM(RegisterLabelOffset(InstructionPointer, errorLabel), ParamRegs.head),
            MovASM(Imm(0), R0, Byte),
            CallASM(PrintFormatted),
            MovASM(Imm(0), ParamRegs.head),
            CallASM(FileFlush),
            MovASM(Imm(-1), ParamRegs.head, Byte),
            CallASM(ExitLabel)
        )
    }
}