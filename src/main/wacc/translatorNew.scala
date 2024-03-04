package wacc

import semanticChecker.SemanticInfo
import IR._
import scala.collection.mutable.ListBuffer
import ast._
import scala.collection.mutable.HashMap
import implicits._

class Translator(val semanticInfo: SemanticInfo, val targetConfig: TargetConfig) {
    import targetConfig._

    var branchCounter: Int = 0
    var asmList: ListBuffer[Line] = ListBuffer.empty
    var stringList: ListBuffer[StringLabel] = ListBuffer.empty
    var funcMap: HashMap[FuncLabel, ListBuffer[Line]] = HashMap.empty


    def translate(): List[Line] = {
        // Generate Header

        asmList += GlobalTag
        if (!stringList.isEmpty) {
            asmList += ReadonlyTag
            asmList ++= stringList
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
        //prog.stats.foreach(translateStatement(_))

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

    def translatePrint(node: Print)(implicit buf: ListBuffer[Line], st: SymbolTable): Unit = {
        // Implement translation for Print statement here
    }

    def translatePrintln(node: Println)(implicit buf: ListBuffer[Line], st: SymbolTable): Unit = {
        // Implement translation for Println statement here
    }

    def translateExit(node: Exit)(implicit buf: ListBuffer[Line], st: SymbolTable): Unit = {
        // Implement translation for Exit statement here

        translateExpression(node.expr)

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
            case v: CharVal =>
            case v: StrVal =>
            case v: BoolVal =>
            case v: PairVal =>
            case v: ArrayVal =>
            case v: IntVal =>
            case _: BinOp => 
            case _: UnOp => 
            
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
}