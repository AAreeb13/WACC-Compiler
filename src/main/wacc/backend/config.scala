package wacc

import IR._
import scala.language.implicitConversions
abstract class TargetConfig {
    val ReturnReg: Register
    val StackPointer: Register
    val BasePointer: Register
    val ArrayPointer: Register
    val IndexPointer: Register
    val ScratchRegs: List[Register]
    val InstructionPointer: Register

    val ParamRegs: List[Register]
    val CallerSaved: List[Register]
    val CalleeSaved: List[Register]

    val allRegs = List(R0, R1, R2, R3, R4, R5, R6, R7, R8, R9, R10, R11, R12, R13, R14, R15)

    implicit def opStr(op: Operand)(implicit size: Size = QWord): String
    implicit def sizeStr(size: Size): String
    implicit def flagStr(flag: Condition): String
}

case object X86Config extends TargetConfig {

    override implicit def sizeStr(size: Size): String = {
        size match {
            case Byte => "b"
            case DWord => "l"
            case QWord => "q"
            case Word => ""
        }
    }

    override implicit def flagStr(flag: Condition): String = {
        flag match {
            case Unconditional => ""
            case NotEqual => "ne"
            case Overflow => "o"
            case Greater => "g"
            case Less => "l"
            case Equal => "e"
            case GreaterEqual => "ge"
            case LessEqual => "le"
        }
    }

    val Rax = R0
    val Rbx = R1
    val Rcx = R2
    val Rdx = R3
    val Rsi = R4
    val Rdi = R5
    val Rsp = R6
    val Rbp = R7
    val Rip = PC

    override val ArrayPointer: Register = R9

    override val IndexPointer: Register = R10

    override val ScratchRegs: List[Register] = List(Rax, Rbx)

    override val ReturnReg: Register = Rax
    override val StackPointer: Register = Rsp
    override val BasePointer: Register = Rbp

    override val InstructionPointer: Register = Rip

    override val ParamRegs: List[Register] = List(Rdi, Rsi, Rdx, Rcx, R8, R9)
    override val CallerSaved: List[Register] = List(Rax, R10, R11) ++ ParamRegs
    override val CalleeSaved: List[Register] = allRegs.diff(CallerSaved)

    


    override implicit def opStr(op: Operand)(implicit size: Size = QWord): String = {
        op match {
            case Imm(value) => s"$$$value"
            case RegisterLabelOffset(reg, offsetLabel) => s"${offsetLabel.name}(${opStr(reg)()})"
            case RegisterRegisterOffset(reg, offsetReg) => s"(${opStr(reg)()}, ${opStr(offsetReg)()})"
            case RegisterOffset(reg) => s"(${opStr(reg)()})"
            case RegisterImmediateOffset(reg, offset) => s"$offset(${opStr(reg)()})"
            case RegisterMultiplierOffset(reg, offsetReg, multiplier) => s"(${opStr(reg)()}, ${opStr(offsetReg)()}, $multiplier)"
            case reg: Register => 
                val regVal = reg match {
                    case Rax => size match {
                        case QWord => "rax"
                        case DWord => "eax"
                        case Word => "ax"
                        case Byte => "al"
                    }
                    case Rbx => size match {
                        case QWord => "rbx"
                        case DWord => "ebx"
                        case Word => "bx"
                        case Byte => "bl"
                    }
                    case Rcx => size match {
                        case QWord => "rcx"
                        case DWord => "cax"
                        case Word => "cx"
                        case Byte => "cl"
                    }
                    case Rdx => size match {
                        case QWord => "rdx"
                        case DWord => "edx"
                        case Word => "dx"
                        case Byte => "dl"
                    }
                    case Rsi => size match {
                        case QWord => "rsi"
                        case DWord => "esi"
                        case Word => "si"
                        case Byte => "sil"
                    }
                    case Rdi => size match {
                        case QWord => "rdi"
                        case DWord => "edi"
                        case Word => "di"
                        case Byte => "dil"
                    }
                    case Rsp => size match {
                        case QWord => "rsp"
                        case DWord => "esp"
                        case Word => "sp"
                        case Byte => "spl"
                    }
                    case Rbp => size match {
                        case QWord => "rbp"
                        case DWord => "ebp"
                        case Word => "bp"
                        case Byte => "bpl"
                    }
                    case Rip => size match {
                        case QWord => "rip"
                        case DWord => "eip"
                        case Word => "undefined"
                        case Byte => "undefined"
                    }
                    case numberedReg => 
                        val suffix = size match {
                            case QWord => ""
                            case DWord => "d"
                            case Word => "w"
                            case Byte => "b"
                        }
                        s"${numberedReg.toString.toLowerCase()}$suffix"
                }
                s"%$regVal"
        }
    }


}

