package wacc

import IR._
import X86Config._

object X86Generator {
    def assemble(ir: List[Line]): String = {
        ir.map { l => 
            l match {
                case StringLabel(name, value) => s"\t.int ${value.length}\n${name}:\n\t.asciz \"${value}\""
                case label: Label => s"${label.name}:"
                case tag: Tag => s".${tag.name}"
                case other =>
                    val repr = other match {
                        case Comment(contents) => s"# $contents"
                        case MovASM(src, dst, flag, size) => flag match {
                            case Unconditional => s"mov${sizeStr(size)} ${opStr(src)(size)}, ${opStr(dst)(size)}"
                            case other => s"cmov${flagStr(flag)} ${opStr(src)(size)}, ${opStr(dst)(size)}"
                        }
                        case MovsASM(src, dst, sizeFrom, sizeTo) => s"movs${sizeStr(sizeFrom)}${sizeStr(sizeTo)} ${opStr(src)(sizeFrom)}, ${opStr(dst)(sizeTo)}"
                        case AddASM(op, _, dst, size) => s"add${sizeStr(size)} ${opStr(op)(size)}, ${opStr(dst)(size)}"
                        case PopASM(op, size) => s"pop${sizeStr(size)} ${opStr(op)(size)}"
                        case AndASM(op, _, dst, size) => s"and${sizeStr(size)} ${opStr(op)(size)}, ${opStr(dst)(size)}"
                        case MulASM(op, _, dst, size) => s"mul${sizeStr(size)} ${opStr(op)(size)}, ${opStr(dst)(size)}"
                        case PushASM(op, size) => s"push${sizeStr(size)} ${opStr(op)(size)}"
                        case CallASM(label) => s"call ${label.name}"
                        case JmpASM(label, flag) => 
                            val flagChar = flag match {
                                case Unconditional => "mp"
                                case other => flagStr(other)
                            }
                            s"j$flagChar $label"
                        case DivASM(_, _, dst, size) => s"cltd\n\tidiv${sizeStr(size)} ${opStr(dst)(size)}"
                        case OrASM(op, _, dst, size) => s"or${sizeStr(size)} ${opStr(op)(size)}, ${opStr(dst)(size)}"
                        case SubASM(op, _, dst, size) => s"sub${sizeStr(size)} ${opStr(op)(size)}, ${opStr(dst)(size)}"
                        case CmpASM(src, dst, size) => s"cmp${sizeStr(size)} ${opStr(src)(size)}, ${opStr(dst)(size)}"
                        case SetASM(dst, flag, size) => s"set${flagStr(flag)} ${opStr(dst)(size)}"
                        case LeaASM(src, dst, size) => s"lea${sizeStr(size)} ${opStr(src)(size)}, ${opStr(dst)(size)}"
                        case RetASM => "ret"
                        case _ => "unreached"
                    }
                    s"\t$repr"
            }
        }.mkString("\n")
    }
}