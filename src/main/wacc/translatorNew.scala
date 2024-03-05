package wacc

import semanticChecker.SemanticInfo
import IR._
import scala.collection.mutable.ListBuffer
import ast._
import scala.collection.mutable.HashMap
import implicits._
import semAst._
import scala.collection.mutable.HashSet

class Translator(val semanticInfo: SemanticInfo, val targetConfig: TargetConfig) {
    import targetConfig._

    var branchCounter: Int = 0
    var asmList: ListBuffer[Line] = ListBuffer.empty
    var stringSet: HashSet[StringLabel] = HashSet.empty
    var funcMap: HashMap[FuncLabel, ListBuffer[Line]] = HashMap.empty


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
        prog.funcs.foreach(f => funcMap.addOne((WaccFuncLabel(f.name), translateFunction(f))))
        funcMap.addOne((MainLabel, translateMain(prog)))
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
            case node: Scope      => translateBlock(node.stats)
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
        buf += CmpASM(TrueImm, ScratchReg)
        buf += JmpASM(trueLabel, Equal)
        translateBlock(node.elseStat)
        buf += JmpASM(endLabel)
        buf += trueLabel
        translateBlock(node.ifStat)
        buf += endLabel

        buf += Comment("End IF")
        
    }

    def translateAssign(node: Assign)(implicit buf: ListBuffer[Line], st: SymbolTable): Unit = {
        // Implement translation for Assign statement here
        
    }

    def translateDeclaration(node: AssignNew)(implicit buf: ListBuffer[Line], st: SymbolTable): Unit = {
        // Implement translation for Declaration statement here
    }

    def translateRead(node: Read)(implicit buf: ListBuffer[Line], st: SymbolTable): Unit = {
        // Implement translation for Read statement here
        val (wrapperLabel, fstring) = (node.enclosingType: @unchecked) match {
            case SemChar => (ReadCharLabel, "%c")
            case SemInt => (ReadIntLabel, "%d")
        }

        if (!funcMap.contains(wrapperLabel)) {
            translateReadLabel(wrapperLabel, fstring, semanticToSize(node.enclosingType))
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
        translateBlock(node.stats)
        buf += doneLabel
        translateExpression(node.cond)
        buf += CmpASM(TrueImm, ScratchReg)
        buf += JmpASM(repeatLabel, Equal)

        buf += Comment("End WHILE")
    }

    def translatePrint(node: Printable)(implicit buf: ListBuffer[Line], st: SymbolTable): Unit = {
        // Implement translation for Print statement here
        translateExpression(node.expr)
        buf += MovASM(ScratchReg, ParamRegs.head)
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
    }

    def translateReturn(node: Return)(implicit buf: ListBuffer[Line], st: SymbolTable): Unit = {
        // Implement translation for Return statement here
        translateExpression(node.expr)
        buf += MovASM(ScratchReg, ReturnReg)
    }

    def translateExpression(expr: Expr)(implicit buf: ListBuffer[Line], st: SymbolTable): Unit = {
        expr match {
            case v: Var => translateVar(v)
            case CharVal(c) => buf += MovASM(Imm(c.toInt), ScratchReg)
            case StrVal(s) =>
                val stringLabel = addStringConstant(s)
                buf += LeaASM(RegisterLabelOffset(InstructionPointer, stringLabel), ScratchReg)
            case BoolVal(b) => buf += MovASM(Imm(if (b) 1 else 0), ScratchReg)
            case PairVal() => buf += MovASM(NullImm, ScratchReg)
            case v: ArrayVal =>
            case IntVal(x) => buf += MovASM(Imm(x), ScratchReg)
            case _: BinOp => 
            case _: UnOp => 
            
        }
    }

    def addStringConstant(string: String): StringLabel = {
        val retVal = StringLabel(s".L.str${stringSet.size}", toRaw(string))
        if (!stringSet.contains(retVal)) stringSet.add(retVal)
        retVal
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
        // Implement translation for Free statement here
    }

    def translatePairCons(node: PairCons)(implicit buf: ListBuffer[Line], st: SymbolTable): Unit = {
        // Implement translation for Free statement here
    }
    
    def translateFuncCall(node: FuncCall)(implicit buf: ListBuffer[Line], st: SymbolTable): Unit = {
        // Implement translation for Free statement here
    }

    def translateVar(node: Var, writeTo: Boolean = false)(implicit buf: ListBuffer[Line], st: SymbolTable): Unit = {
    }

    def translateArrayElem(node: ArrayVal, writeTo: Boolean = false)(implicit buf: ListBuffer[Line], st: SymbolTable): Unit = {
    }

    def translatePairElem(node: PairElem, writeTo: Boolean = false)(implicit buf: ListBuffer[Line], st: SymbolTable): Unit = {
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

    def toRaw(s: String): String =
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
                val trueStringLabel = StringLabel(s".L.${label.name}_string_true", fstring)
                val falseStringLabel = StringLabel(s".L.${label.name}_string_false", fstring)

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
}