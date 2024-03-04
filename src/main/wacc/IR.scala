package wacc

object IR {
    sealed trait Line

    sealed trait Operand

    case class Imm(value: Int) extends Operand

    sealed trait Location extends Operand

    sealed trait Register extends Location {
        val size: Size = QWord
    }

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

    case object Auto extends Size

    sealed class Label(name: String) extends Line

    case class JumpLabel(name: String) extends Label(name)
    case class StringLabel(name: String, value: String) extends Label(name)

    sealed trait FuncLabel extends Label

    case class WaccFuncLabel(name: String) extends Label(name) with FuncLabel
    sealed class LibFuncLabel(name: String) extends Label(name) with FuncLabel
    case class WrapperFuncLabel(name: String) extends Label(name) with FuncLabel

    case object MainLabel extends Label("main") with FuncLabel

    case object MallocLabel    extends LibFuncLabel("malloc")
    case object ExitLabel      extends LibFuncLabel("exit")
    case object FreeLabel      extends LibFuncLabel("free")
    case object PrintFormatted extends LibFuncLabel("printf")
    case object ScanFormatted  extends LibFuncLabel("scanf")
    case object FileFlush      extends LibFuncLabel("fflush")
    case object Puts           extends LibFuncLabel("puts")

    case object PrintlnLabel      extends Label("_println") with FuncLabel
    case object PrintIntLabel     extends Label("_printi") with FuncLabel
    case object PrintBoolLabel    extends Label("_printb") with FuncLabel
    case object PrintCharLabel    extends Label("_printc") with FuncLabel
    case object PrintStrLabel     extends Label("_prints") with FuncLabel
    case object PrintPtrLabel     extends Label("_printp") with FuncLabel

    case object ReadIntLabel  extends Label("_readi") with FuncLabel
    case object ReadCharLabel extends Label("_readc") with FuncLabel

    case object CheckNullLabel     extends Label("_errNull") with FuncLabel
    case object CheckOverflowLabel extends Label("_errOverflow") with FuncLabel
    case object CheckDivZeroLabel  extends Label("_errDivZero") with FuncLabel
    case object CheckBoundLabel    extends Label("_boundsCheck") with FuncLabel

    case object ArrayStoreLabel  extends Label("_arrStore") with FuncLabel
    case object ArrayStoreBLabel extends Label("_arrStoreB") with FuncLabel
    case object ArrayLoadLabel   extends Label("_arrLoad") with FuncLabel
    case object ArrayLoadBLabel  extends Label("_arrLoadB") with FuncLabel
    case object FreePairLabel    extends Label("_freepair") with FuncLabel
    case object FreeArrayLabel   extends Label("_free") with FuncLabel

    case object RetASM extends Instruction

    sealed trait Instruction extends Line

    case class PushASM(op: Operand) extends Instruction
    case class PopASM(op: Operand) extends Instruction // although immediates should not be popped to
    
    case class SubASM(op1: Operand, op2: Operand, dst: Location) extends Instruction
    case class MulASM(op1: Operand, op2: Operand, dst: Location) extends Instruction
    case class DivASM(op1: Operand, op2: Operand, dst: Location) extends Instruction
    case class AddASM(op1: Operand, op2: Operand, dst: Location) extends Instruction
    case class AndASM(op1: Operand, op2: Operand, dst: Location) extends Instruction
    case class OrASM(op1: Operand, op2: Operand, dst: Location) extends Instruction

    case class MovASM private (src: Operand, dst: Location, flag: Option[Condition], size: Option[Size]) extends Instruction

    object MovASM {
        def apply(src: Operand, dst: Location, flag: Condition): MovASM = 
            MovASM(src, dst, Some(flag), None)
        
        def apply(src: Operand, dst: Location, size: Size = Auto): MovASM = 
            MovASM(src, dst, None, Some(size))
    }

    case class CmpASM(src: Operand, dst: Location) extends Instruction
    case class SetASM(dst: Operand, flag: Condition) extends Instruction
    case class JmpASM(label: JumpLabel, flag: Condition = Unconditional) extends Instruction
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

    sealed class Tag(name: String) extends Line
    case object TextTag   extends Tag("text")
    case object GlobalTag extends Tag("global main")
    case object ReadonlyTag  extends Tag("section readonly")

    val TrueImm = Imm(1)
    val FalseImm = Imm(0)
    val AlignmentMaskImm = Imm(-16)
    val DefaultExitCode = Imm(0)

}