package wacc

import implicits.sizeToInt

object IR {
    sealed trait Line

    sealed trait Operand

    case class Imm(value: BigInt) extends Operand

    sealed trait Location extends Operand

    sealed trait Register extends Location {
        val size: Size = QWord
    }

    case object PC extends Register
    case object R0 extends Register
    case object R1 extends Register
    case object R2 extends Register
    case object R3 extends Register
    case object R4 extends Register
    case object R5 extends Register
    case object R6 extends Register
    case object R7 extends Register
    case object R8 extends Register
    case object R9 extends Register
    case object R10 extends Register
    case object R11 extends Register
    case object R12 extends Register
    case object R13 extends Register
    case object R14 extends Register
    case object R15 extends Register

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

    //case object Auto extends Size

    sealed class Label(val name: String) extends Line

    case class JumpLabel(override val name: String) extends Label(name)
    case class StringLabel(override val name: String, value: String) extends Label(name)

    sealed trait FuncLabel extends Label

    case class WaccFuncLabel(_name: String) extends Label(_name) with FuncLabel {
        override val name = s"wacc_${_name}"
    }
    sealed class LibFuncLabel(override val name: String) extends Label(name) with FuncLabel
    sealed class WrapperFuncLabel(override val name: String) extends Label(name) with FuncLabel

    case object MainLabel extends Label("main") with FuncLabel

    case object MallocLabel    extends LibFuncLabel("malloc")
    case object ExitLabel      extends LibFuncLabel("exit")
    case object FreeLabel      extends LibFuncLabel("free")
    case object PrintFormatted extends LibFuncLabel("printf")
    case object ScanFormatted  extends LibFuncLabel("scanf")
    case object FileFlush      extends LibFuncLabel("fflush")
    case object Puts           extends LibFuncLabel("puts")

    case object PrintlnLabel      extends WrapperFuncLabel("_println")
    case object PrintIntLabel     extends WrapperFuncLabel("_printi")
    case object PrintBoolLabel    extends WrapperFuncLabel("_printb")
    case object PrintCharLabel    extends WrapperFuncLabel("_printc")
    case object PrintStrLabel     extends WrapperFuncLabel("_prints")
    case object PrintPtrLabel     extends WrapperFuncLabel("_printp")

    case object ReadIntLabel  extends WrapperFuncLabel("_readi")
    case object ReadCharLabel extends WrapperFuncLabel("_readc")
    case object ExitWrapperLabel extends WrapperFuncLabel("_exit")
    case object MallocWrapperLabel extends WrapperFuncLabel("_malloc")

    case object CheckNullLabel     extends WrapperFuncLabel("_errNull")
    case object CheckOverflowLabel extends WrapperFuncLabel("_errOverflow")
    case object CheckDivZeroLabel  extends WrapperFuncLabel("_errDivZero")
    case object CheckBoundsLabel    extends WrapperFuncLabel("_boundsCheck")
    case object CheckBadCharLabel  extends WrapperFuncLabel("_errBadChar")
    case object CheckOOMLabel     extends WrapperFuncLabel("_errOutOfMemory")

    case class ArrayStoreLabel(size: Size)  extends WrapperFuncLabel("_arrStore") {
        override val name = s"_arrStore${sizeToInt(size)}"
    }
    
    case class ArrayLoadLabel(size: Size)   extends WrapperFuncLabel("_arrLoad") {
        override val name = s"_arrLoad${sizeToInt(size)}"
    }

    case object ArrayStoreBLabel extends WrapperFuncLabel("_arrStoreB")
    case object ArrayLoadBLabel  extends WrapperFuncLabel("_arrLoadB")
    case object FreePairLabel    extends WrapperFuncLabel("_freepair")
    case object FreeArrayLabel   extends WrapperFuncLabel("_free")

    case object RetASM extends Instruction

    sealed trait Instruction extends Line

    case class PushASM(op: Operand, size: Size = QWord) extends Instruction
    case class PopASM(op: Operand, size: Size = QWord) extends Instruction // although immediates should not be popped to
    
    case class SubASM(op1: Operand, op2: Operand, dst: Location, size: Size = QWord) extends Instruction
    case class MulASM(op1: Operand, op2: Operand, dst: Location, size: Size = DWord) extends Instruction
    case class DivASM(op1: Operand, op2: Operand, dst: Location, size: Size = DWord) extends Instruction
    case class AddASM(op1: Operand, op2: Operand, dst: Location, size: Size = QWord) extends Instruction
    case class AndASM(op1: Operand, op2: Operand, dst: Location, size: Size = QWord) extends Instruction
    case class OrASM(op1: Operand, op2: Operand, dst: Location, size: Size = QWord) extends Instruction

    case class TestASM(src: Operand, dst: Operand, size: Size = QWord) extends Instruction // todo: check if src/dst are operands or locations

    case class MovsASM(src: Operand, dst: Location, sizeFrom: Size, sizeTo: Size = QWord) extends Instruction
    case class MovASM(src: Operand, dst: Location, flag: Condition, size: Size) extends Instruction

    object MovASM {
        def apply(src: Operand, dst: Location, flag: Condition): MovASM = 
            MovASM(src, dst, flag, DWord)
        
        def apply(src: Operand, dst: Location, size: Size = QWord): MovASM = 
            MovASM(src, dst, Unconditional, size)
    }

    case class CmpASM(src: Operand, dst: Location, size: Size = QWord) extends Instruction
    case class SetASM(dst: Operand, flag: Condition, size: Size = Byte) extends Instruction
    case class JmpASM(label: Label, flag: Condition = Unconditional) extends Instruction
    case class LeaASM(src: Operand, dst: Register, size: Size = QWord) extends Instruction
    case class CallASM(label: FuncLabel) extends Instruction

    sealed trait Condition
    case object Greater extends Condition
    case object GreaterEqual extends Condition
    case object Equal extends Condition
    case object NotEqual extends Condition
    case object Less extends Condition
    case object LessEqual extends Condition
    case object Overflow extends Condition
    case object Unconditional extends Condition

    case class Comment(contents: String) extends Line

    sealed class Tag(val name: String) extends Line
    case object TextTag   extends Tag("text")
    case object GlobalTag extends Tag("globl main")
    case object ReadonlyTag  extends Tag("section .rodata")

    val TrueImm = Imm(1)
    val FalseImm = Imm(0)
    val NullImm = Imm(0)
    val AlignmentMaskImm = Imm(-16)
    val ReadOffsetImm = Imm(16)
    val DefaultExitCode = Imm(0)

}