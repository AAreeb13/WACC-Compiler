package wacc

import IR._

object X86Generator {
    def assemble(ir: List[Line]): String = {
        ir.map { l => 
            l match {
                case label: Label => s"${label.name}:"
                case tag: Tag => s".${tag.name}"
                case other =>
                    val repr = other match {
                        case Comment(contents) => s"# $contents"
                        case MovASM(src, dst, Some(flag), size) => s"cmov$flag $src, $dst"
                        case MovASM(src, dst, flag, Some(size)) => s"mov$size $src, $dst"
                        case AddASM(op1, op2, dst) => s"add $op1, $dst"
                        case PopASM(op) => s"pop $op"
                        case AndASM(op1, op2, dst) => s"and $op1, $dst"
                        case MulASM(op1, op2, dst) => s"add $op1, $dst"
                        case PushASM(op) => s"push $op"
                        case CallASM(label) => s"call ${label.name}"
                        case JmpASM(label, flag) => 
                            val flagChar = flag match {
                                case GreaterEqual => "ge"
                                case Greater => "g"
                                case NotEqual => "ne"
                                case Equal => "e"
                                case Overflow => "o"
                                case LessEqual => "le"
                                case Unconditional => "mp"
                                case Less => "l"
                            }
                            s"j$flagChar $label"
                        case DivASM(op1, op2, dst) => s"cltd\n\tidivl $dst"
                        case OrASM(op1, op2, dst) => s"or $op1, $dst"
                        case SubASM(op1, op2, dst) => s"sub $op1, $dst"
                        case CmpASM(src, dst) => s"cmp $src, $dst"
                        case SetASM(dst, flag) => s"set$flag $dst"
                        case RetASM => "ret"
                        case _ => "unreached"
                    }
                    s"\t$repr"
            }
        }.mkString("\n")
    }
}