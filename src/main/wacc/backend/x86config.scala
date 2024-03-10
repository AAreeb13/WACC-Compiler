package wacc


import IR._
import scala.language.implicitConversions

/*
 * This object specifies how registers, sizes and operations 
 * are represented in x86 ATT architecutre
 */

case object X86Config extends TargetConfig {
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
    override val DivRegister: Register = Rax
    override val ModRegister: Register = R3
    
    override val ArrayPointerPointer: Register = R15

    override val IndexPointer: Register = R10

    override val ScratchRegs: List[Register] = List(Rax, Rbx)

    override val ReturnReg: Register = Rax
    override val StackPointer: Register = Rsp
    override val BasePointer: Register = Rbp

    override val InstructionPointer: Register = Rip

    override val ParamRegs: List[Register] = List(Rdi, Rsi, Rdx, Rcx, R8, R9)
    override val CallerSaved: List[Register] = List(Rax, R10, R11) ++ ParamRegs
    override val CalleeSaved: List[Register] = allRegs.diff(CallerSaved)

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


    override def assemble(ir: List[Line]): String = {
        ir.map { l => 
            l match {
                case EmptyLine => ""
                case StringLabel(name, value) => s"\t.int ${value.length}\n${name}:\n\t.asciz \"${value}\""
                case label: Label => s"${label.name}:"
                case tag: Tag =>
                    val prefix = "."
                    val name = tag match {
                        case TextTag => "text"
                        case GlobalTag => "globl main"
                        case ReadonlyTag => "section .rodata"
                    }
                    s"$prefix$name"
                case other =>
                    val repr = other match {
                        case Comment(contents) => s"# ${utils.toRaw(contents)}"
                        case MovASM(src, dst, flag, size) => flag match {
                            case Unconditional => s"mov${sizeStr(size)} ${opStr(src)(size)}, ${opStr(dst)(size)}"
                            case other => s"cmov${flagStr(flag)} ${opStr(src)(size)}, ${opStr(dst)(size)}"
                        }
                        case MovsASM(src, dst, sizeFrom, sizeTo) => s"movs${sizeStr(sizeFrom)}${sizeStr(sizeTo)} ${opStr(src)(sizeFrom)}, ${opStr(dst)(sizeTo)}"
                        case AddASM(op, _, dst, size) => s"add${sizeStr(size)} ${opStr(op)(size)}, ${opStr(dst)(size)}"
                        case PopASM(op, size) => s"pop${sizeStr(size)} ${opStr(op)(size)}"
                        case AndASM(op, _, dst, size) => s"and${sizeStr(size)} ${opStr(op)(size)}, ${opStr(dst)(size)}"
                        case NotASM(src, _, size) => s"not${sizeStr(size)} ${opStr(src)(size)}"
                        case MulASM(op1, op2, dst, size) => 
                            if (op2 == dst) s"imul${sizeStr(size)} ${opStr(op1)(size)}, ${opStr(dst)(size)}"
                            else s"imul${sizeStr(size)} ${opStr(op1)(size)}, ${opStr(op2)(size)}, ${opStr(dst)(size)}"
                        case PushASM(op, size) => s"push${sizeStr(size)} ${opStr(op)(size)}"
                        case CallASM(label) => 
                            val suffix = label match {
                                case _: LibFuncLabel => "@plt"
                                case _ => ""
                            }
                            s"call ${label.name}$suffix"
                        case JmpASM(label, flag) => 
                            val flagChar = flag match {
                                case Unconditional => "mp"
                                case other => flagStr(other)
                            }
                            s"j$flagChar ${label.name}"
                        case DivASM(_, _, dst, size) => s"cltd\n\tidiv${sizeStr(size)} ${opStr(dst)(size)}"
                        case OrASM(op, _, dst, size) => s"or${sizeStr(size)} ${opStr(op)(size)}, ${opStr(dst)(size)}"
                        case SubASM(op, _, dst, size) => s"sub${sizeStr(size)} ${opStr(op)(size)}, ${opStr(dst)(size)}"
                        case CmpASM(src, dst, size) => s"cmp${sizeStr(size)} ${opStr(src)(size)}, ${opStr(dst)(size)}"
                        case SetASM(dst, flag, size) => s"set${flagStr(flag)} ${opStr(dst)(size)}"
                        case LeaASM(src, dst, size) => s"lea${sizeStr(size)} ${opStr(src)(size)}, ${opStr(dst)(size)}"
                        case TestASM(src, dst, size) => s"test${sizeStr(size)} ${opStr(src)(size)}, ${opStr(dst)(size)}"
                        case RetASM => "ret"
                        case _ => "unreached"
                    }
                    s"\t$repr"
            }
        }.mkString("\n")
    }
}