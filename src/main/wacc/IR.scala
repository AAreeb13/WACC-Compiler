package wacc

import parsley.internal.machine.instructions.Instr
import java.util.concurrent.locks.Condition

object IR {
    sealed trait Line

    sealed trait Operand

    case class Imm(value: Int) extends Operand

    sealed trait Location extends Operand

    sealed trait Register extends Location {
        val size: Size
    }

    case class R1(size: Size) extends Register
    case class R2(size: Size) extends Register
    case class R3(size: Size) extends Register
    case class R4(size: Size) extends Register
    case class R5(size: Size) extends Register
    case class R6(size: Size) extends Register
    case class R7(size: Size) extends Register
    case class R8(size: Size) extends Register
    case class R8(size: Size) extends Register
    case class R10(size: Size) extends Register
    case class R11(size: Size) extends Register
    case class R12(size: Size) extends Register
    case class R13(size: Size) extends Register
    case class R14(size: Size) extends Register
    case class R15(size: Size) extends Register

    sealed trait Memory extends Location
    case class RegisterOffset(reg: Register) extends Memory
    case class RegisterImmediateOffset(reg: Register, offset: Int) extends Memory
    case class RegisterLabelOffset(reg: Register, offsetLabel: Label) extends Memory
    case class RegisterRegisterOffset(reg: Register, offsetReg: Register) extends Memory
    case class RegisterMultiplierOffset(reg: Register, offsetReg: Register, multiplier: Int) extends Memory


    sealed trait Size
    case object Byte extends Size
    case object Word extends Size
    case object DWord extends Size
    case object QWord extends Size

    case object Auto extends Size

    sealed class Label(name: String) extends Line

    case class JumpLabel(name: String) extends Label(name)
    case class StringLabel(name: String) extends Label(name)

    sealed trait FuncLabel extends Label

    case class WaccFuncLabel(name: String) extends Label(name) with FuncLabel
    case class LibFuncLabel(name: String) extends Label(name) with FuncLabel
    case class WrapperFuncLabel(name: String) extends Label(name) with FuncLabel

    case object Main extends Label("main") with FuncLabel

    case object MallocLabel    extends LibFuncLabel("malloc")
    case object ExitLabel      extends LibFuncLabel("exit")
    case object FreeLabel      extends LibFuncLabel("free")
    case object PrintFormatted extends LibFuncLabel("printf")
    case object ScanFormatted  extends LibFuncLabel("scanf")
    case object FileFlush      extends LibFuncLabel("fflush")
    case object Puts           extends LibFuncLabel("puts")

    case object Println      extends Label("_println") with FuncLabel
    case object PrintInt     extends Label("_printi") with FuncLabel
    case object PrintBool    extends Label("_printb") with FuncLabel
    case object PrintChar    extends Label("_printc") with FuncLabel
    case object PrintStr     extends Label("_prints") with FuncLabel
    case object PrintPtr     extends Label("_printp") with FuncLabel

    case object ReadInt  extends Label("_readi") with FuncLabel
    case object ReadChar extends Label("_readc") with FuncLabel

    case object CheckNull     extends Label("_errNull") with FuncLabel
    case object CheckOverflow extends Label("_errOverflow") with FuncLabel
    case object CheckDivZero  extends Label("_errDivZero") with FuncLabel
    case object CheckBound    extends Label("_boundsCheck") with FuncLabel

    case object ArrayStore  extends Label("_arrStore") with FuncLabel
    case object ArrayStoreB extends Label("_arrStoreB") with FuncLabel
    case object ArrayLoad   extends Label("_arrLoad") with FuncLabel
    case object ArrayLoadB  extends Label("_arrLoadB") with FuncLabel
    case object FreePair    extends Label("_freepair") with FuncLabel
    case object FreeArray   extends Label("_free") with FuncLabel

    sealed trait Instruction extends Line

    case class Push(op: Operand) extends Instruction
    case class Pop(op: Operand) extends Instruction // although immediates should not be popped to
    
    case class Sub(op1: Operand, op2: Operand, dst: Location) extends Instruction
    case class Mul(op1: Operand, op2: Operand, dst: Location) extends Instruction
    case class Div(op1: Operand, op2: Operand, dst: Location) extends Instruction
    case class Add(op1: Operand, op2: Operand, dst: Location) extends Instruction
    case class And(op1: Operand, op2: Operand, dst: Location) extends Instruction
    case class Mul(op1: Operand, op2: Operand, dst: Location) extends Instruction
    case class Or(op1: Operand, op2: Operand, dst: Location) extends Instruction

    case class Mov private (src: Operand, dst: Location, flag: Option[Condition], size: Option[Size]) extends Instruction

    object Mov {
        override def apply(src: Operand, dst: Location, flag: Condition): Mov = 
            Mov(src, dst, Some(flag), None)
        
        override def apply(src: Operand, dst: Location, size: Size = Auto): Mov = 
            Mov(src, dst, None, Some(flag))
    }

    case class Cmp(src: Operand, dst: Location) extends Instruction
    case class Set(dst: Operand, flag: Condition) extends Instruction
    case class Jmp(label: JumpLabel, flag: Condition = Unconditional) extends Instruction
    case class Call(label: FuncLabel) extends Instruction


}