package wacc

import implicits.sizeToInt

/*
 * This file contains the Intermediate representaion for encapsulating
 * all the necessary information needed to formulate the assembly code
 * 
 * It is generic, meaning it is platform independent
 * This can be used to generate for any platform
 * 
 * It defines key information such as registers, operands, labels, conditions
 */

object IR {
    sealed trait Line

    sealed trait Operand

    sealed trait NonMemory extends Operand
    sealed trait Location extends Operand

    case class Imm(value: BigInt) extends Operand with NonMemory

    sealed trait Register extends Location with NonMemory {
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

    object Memory {
        def apply(reg: Register): Memory = new RegisterOffset(reg)
        def apply(reg: Register, offset: Int): Memory = new RegisterImmediateOffset(reg, offset)
        def apply(reg: Register, offsetLabel: Label): Memory = new RegisterLabelOffset(reg, offsetLabel)
        def apply(reg: Register, offsetReg: Register): Memory = new RegisterRegisterOffset(reg, offsetReg)
        def apply(reg: Register, offsetReg: Register, multiplier: Int): Memory = new RegisterMultiplierOffset(reg, offsetReg, multiplier)
    }

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

    case object FreePairLabel    extends WrapperFuncLabel("_freepair")
    case object FreeArrayLabel   extends WrapperFuncLabel("_free")

    case object RetASM extends Instruction

    sealed trait Instruction extends Line

    case class PushASM(op: Location, size: Size = QWord) extends Instruction
    case class PopASM(op: Location, size: Size = QWord) extends Instruction
    
    case class SubASM(op1: NonMemory, op2: NonMemory, dst: Register, size: Size = QWord) extends Instruction
    case class MulASM(op1: NonMemory, op2: NonMemory, dst: Register, size: Size = DWord) extends Instruction
    case class DivASM(op1: NonMemory, op2: NonMemory, dst: Register, size: Size = DWord) extends Instruction
    case class AddASM(op1: NonMemory, op2: NonMemory, dst: Register, size: Size = QWord) extends Instruction
    case class AndASM(op1: NonMemory, op2: NonMemory, dst: Register, size: Size = QWord) extends Instruction
    case class OrASM(op1: NonMemory, op2: NonMemory, dst: Register, size: Size = QWord) extends Instruction

    abstract class NonDoubleMem(src: Operand, dst: Operand) extends Instruction {
        require((src, dst) match {
            case (_: Memory, _: Memory) => false
            case _ => true
        }, "Either src or dst can be an instance of a memory location, but not both")
    }

    abstract class NonDoubleImm(src: Operand, dst: Operand) extends Instruction {
        require((src, dst) match {
            case (_: Imm, _: Imm) => false
            case _ => true
        }, s"Either src or dst can be an instance of an immediate value, but not both")
    }

    case class TestASM(src: Operand, dst: Operand, size: Size = QWord) extends NonDoubleMem(src, dst) // todo: check if src/dst are operands or locations

    case class MovsASM(src: Operand, dst: Location, sizeFrom: Size, sizeTo: Size = QWord) extends NonDoubleMem(src, dst)
    case class MovASM(src: Operand, dst: Location, flag: Condition, size: Size) extends NonDoubleMem(src, dst)

    object MovASM {
        def apply(src: Operand, dst: Location, flag: Condition): MovASM = 
            MovASM(src, dst, flag, DWord)
        
        def apply(src: Operand, dst: Location, size: Size = QWord): MovASM = 
            MovASM(src, dst, Unconditional, size)
    }

    case class CmpASM(src: Operand, dst: Location, size: Size = QWord) extends NonDoubleImm(src, dst)
    case class SetASM(dst: Operand, flag: Condition, private val size: Size = Byte) extends Instruction

    case class JmpASM(label: Label, flag: Condition = Unconditional) extends Instruction
    case class LeaASM(src: Memory, dst: Register, size: Size = QWord) extends Instruction
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

    object Comment {
        def apply(contents: String): Comment = 
            new Comment(utils.toRaw(contents, false))
    }

    sealed trait Tag extends Line
    case object TextTag   extends Tag
    case object GlobalTag extends Tag
    case object ReadonlyTag  extends Tag

    val TrueImm = Imm(1)
    val FalseImm = Imm(0)
    val NullImm = Imm(0)
    val AlignmentMaskImm = Imm(-16)
    val ReadOffsetImm = Imm(16)
    val DefaultExitCode = Imm(0)
    val ExitFailureImm = Imm(-1)

}