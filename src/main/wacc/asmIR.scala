package wacc

object asmIR {

object ComparisonType extends Enumeration {
    type Flag = Value
    val Greater = Value("g")
    val GreaterEqual = Value("ge")
    val Equal = Value("e")
    val NotEqual = Value("ne")
    val Less = Value("l")
    val LessEqual = Value("le")
    val Overflow = Value("o")
    val Unconditional = Value
}

object InstructionSize extends Enumeration {
    type Size = Value
    val Byte = Value("b")
    val Word = Value("")
    val DWord = Value("l")
    val QWord = Value("q")
}

import InstructionSize._
import ComparisonType._

sealed trait ASMItem {
    def format: String = ???
}

sealed trait Callable

case class Label(ident: String) extends ASMItem with Callable {
    override def toString() = ident
}

object LibFunc extends Enumeration {
    type Func = Value with Callable
    case class LibFuncValue(name: String) extends Val(name) with Callable {
        override def toString = super.toString()
    }
    val Printf = LibFuncValue("printf@plt")
    val Scanf = LibFuncValue("scanf@plt")
    val Malloc = LibFuncValue("malloc@plt")
    val Free = LibFuncValue("free@plt")
    val Exit = LibFuncValue("exit@plt")
    val Flush = LibFuncValue("fflush@plt")
    val Puts = LibFuncValue("puts@plt")
}

case class Comment(contents: String) extends ASMItem {
    override def toString() = s"# $contents\n"
}

sealed trait Section extends ASMItem

case object Readonly extends Section {
    override def toString() = ".section .rodata"
}
case object Text extends Section {
    override def toString() = ".text"
}
case object Global extends Section {
    override def toString() = ".globl main"
}

case class StringDecl(strVal: String, label: Label) extends Section {
    override def toString() =
        Comment(s"length of ${label.toString}").toString() +
        s"\t.int ${strVal.length}\n" + 
        label.toString + ": \n" + 
        s"\t.asciz \"${strVal}\""
}

sealed trait Operand extends ASMItem

case class ImmVal(value: BigInt) extends Operand {
    override def toString() = s"$$$value"
}

case class Mem(reg: Reg, offset: Option[(ASMItem, Option[Int])]) extends Operand {
    override def toString: String = offset match {
        case None                               => s"($reg)"
        case Some((ImmVal(value), None))        => s"$value($reg)"
        case Some((Label(ident), None))         => s"$ident($reg)"
        case Some((offsetReg: Reg, None))       => s"($reg, $offsetReg)"
        case Some((offsetReg: Reg, Some(mul)))  => s"($reg, $offsetReg, $mul)"
        case _ => "error"
    }
}

object Mem {
    def apply(reg: Reg): Mem = Mem(reg, None)
    def apply(reg: Reg, offsetOp: ASMItem): Mem = Mem(reg, Some((offsetOp, None)))
    def apply(reg: Reg, offsetOp: ASMItem, multiplier: Int): Mem = Mem(reg, Some((offsetOp, Some(multiplier))))
}

case class Test(op1: Operand, op2: Operand, size: Size = QWord) extends Instr {
    override def toString() = s"test${size} ${op1}, ${op2}"
}

sealed trait Instr extends ASMItem

case object Ret extends Instr {
    override def toString() = "ret"
}
case class Mov(src: Operand, dst: Operand, size: Size = QWord) extends Instr {
    override def toString() = s"mov${size} ${src}, ${dst}"
}
case class Movs(src: Operand, dst: Operand, sizeFrom: Size, sizeTo: Size = QWord) extends Instr {
    override def toString() = s"movs${sizeFrom}${sizeTo} ${src}, ${dst}"
}

case class Pop(reg: Operand, size: Size = QWord) extends Instr {
    override def toString() = s"pop${size} ${reg}"
}
case class Push(reg: Operand, size: Size = QWord) extends Instr {
    override def toString() = s"push${size} ${reg}"
}

case class Set(reg: Operand) {
    override def toString() = s"set ${reg}"
}

case class CMov(src: Operand, dst: Operand, flag: Flag) extends Instr {
    override def toString() = s"cmov${flag} ${src}, ${dst}"
}
case class Jmp(label: Label, flag: Flag = Unconditional) extends Instr {
    override def toString() = flag match {
        case Unconditional => s"jmp ${label}"
        case other => s"j$flag ${label}"
    }
}

case class Call(callable: Callable) extends Instr {
    override def toString() = s"call ${callable}"
}

case class Cmp(src: Operand, dst: Operand, size: Size = QWord) extends Instr {
    override def toString() = s"cmp${size} ${src}, ${dst}"
}
case class Lea(src: Operand, dst: Operand, size: Size = QWord) extends Instr {
    override def toString() = s"lea${size} ${src}, ${dst}"
}

case class Add(src: Operand, dst: Operand, size: Size = QWord) extends Instr {
    override def toString() = s"add${size} ${src}, ${dst}"
}
case class Sub(src: Operand, dst: Operand, size: Size = QWord) extends Instr {
    override def toString() = s"sub${size} ${src}, ${dst}"
}
case class And(src: Operand, dst: Operand, size: Size = QWord) extends Instr {
    override def toString() = s"and${size} ${src}, ${dst}"
}
case class IMul(src: Operand, dst: Operand, size: Size = QWord) extends Instr {
    override def toString() = s"imul${size} ${src}, ${dst}"
}
case class IDiv(op: Operand, size: Size = QWord) extends Instr {
    override def toString() = "cltd\n" + s"\tidiv${size} ${op}"
}

object RegisterNames extends Enumeration {
  type Name = Value
  val Rip, Rax, Rbx, Rcx, Rdx, Rsi, Rdi, Rbp, Rsp, R8, R9, R10, R11, R12, R13, R14, R15 = Value
}

import RegisterNames._

case class Reg(name: Name, size: Size = QWord) extends Operand {
  
  def toSize(newSize: Size): Reg = Reg(name, newSize)

  override def toString: String = (name, size) match {
    case (Rip, QWord) => "%rip"
    case (Rip, DWord) => "%eip"

    case (Rax, Byte) => "%al"
    case (Rax, Word) => "%ax"
    case (Rax, DWord) => "%eax"
    case (Rax, QWord) => "%rax"

    case (Rbx, Byte) => "%bl"
    case (Rbx, Word) => "%bx"
    case (Rbx, DWord) => "%ebx"
    case (Rbx, QWord) => "%rbx"

    case (Rcx, Byte) => "%cl"
    case (Rcx, Word) => "%cx"
    case (Rcx, DWord) => "%ecx"
    case (Rcx, QWord) => "%rcx"

    case (Rdx, Byte) => "%dl"
    case (Rdx, Word) => "%dx"
    case (Rdx, DWord) => "%edx"
    case (Rdx, QWord) => "%rdx"

    case (Rsi, Byte) => "%sil"
    case (Rsi, Word) => "%si"
    case (Rsi, DWord) => "%esi"
    case (Rsi, QWord) => "%rsi"

    case (Rdi, Byte) => "%dil"
    case (Rdi, Word) => "%di"
    case (Rdi, DWord) => "%edi"
    case (Rdi, QWord) => "%rdi"

    case (Rbp, Byte) => "%bpl"
    case (Rbp, Word) => "%bp"
    case (Rbp, DWord) => "%ebp"
    case (Rbp, QWord) => "%rbp"

    case (Rsp, Byte) => "%spl"
    case (Rsp, Word) => "%sp"
    case (Rsp, DWord) => "%esp"
    case (Rsp, QWord) => "%rsp"

    case (name, Byte) => s"%${name.toString().toLowerCase()}b"
    case (name, Word) => s"%${name.toString().toLowerCase()}w"
    case (name, DWord) => s"%${name.toString().toLowerCase()}d"
    case (name, QWord) => s"%${name.toString().toLowerCase()}"

    case _ => "error"
  }
}


// sealed trait Reg extends Operand {
//   val sizeType: InstrSize.Value
// }

// case class Rax(sizeType: InstrSize.Size = InstrSize.QWord) extends Reg {
//   override def toString: String = sizeType match {
//     case InstrSize.Byte => "%al"
//     case InstrSize.Word => "%ax"
//     case InstrSize.DWord => "%eax"
//     case InstrSize.QWord => "%rax"
//   }
// }

// case class Rbx(sizeType: InstrSize.Size = InstrSize.QWord) extends Reg {
//   override def toString: String = sizeType match {
//     case InstrSize.Byte => "%bl"
//     case InstrSize.Word => "%bx"
//     case InstrSize.DWord => "%ebx"
//     case InstrSize.QWord => "%rbx"
//   }
// }

// case class Rcx(sizeType: InstrSize.Size = InstrSize.QWord) extends Reg {
//   override def toString: String = sizeType match {
//     case InstrSize.Byte => "%cl"
//     case InstrSize.Word => "%cx"
//     case InstrSize.DWord => "%ecx"
//     case InstrSize.QWord => "%rcx"
//   }
// }

// case class Rdx(sizeType: InstrSize.Size = InstrSize.QWord) extends Reg {
//   override def toString: String = sizeType match {
//     case InstrSize.Byte => "%dl"
//     case InstrSize.Word => "%dx"
//     case InstrSize.DWord => "%edx"
//     case InstrSize.QWord => "%rdx"
//   }
// }

// case class Rsi(sizeType: InstrSize.Size = InstrSize.QWord) extends Reg {
//   override def toString: String = sizeType match {
//     case InstrSize.Byte => "%sil"
//     case InstrSize.Word => "%si"
//     case InstrSize.DWord => "%esi"
//     case InstrSize.QWord => "%rsi"
//   }
// }

// case class Rdi(sizeType: InstrSize.Size = InstrSize.QWord) extends Reg {
//   override def toString: String = sizeType match {
//     case InstrSize.Byte => "%dil"
//     case InstrSize.Word => "%di"
//     case InstrSize.DWord => "%edi"
//     case InstrSize.QWord => "%rdi"
//   }
// }

// case class Rbp(sizeType: InstrSize.Size = InstrSize.QWord) extends Reg {
//   override def toString: String = sizeType match {
//     case InstrSize.Byte => "%bpl"
//     case InstrSize.Word => "%bp"
//     case InstrSize.DWord => "%ebp"
//     case InstrSize.QWord => "%rbp"
//   }
// }

// case class Rsp(sizeType: InstrSize.Size = InstrSize.QWord) extends Reg {
//   override def toString: String = sizeType match {
//     case InstrSize.Byte => "%spl"
//     case InstrSize.Word => "%sp"
//     case InstrSize.DWord => "%esp"
//     case InstrSize.QWord => "%rsp"
//   }
// }

// case class R8(sizeType: InstrSize.Size = InstrSize.QWord) extends Reg {
//   override def toString: String = sizeType match {
//     case InstrSize.Byte => "%r8b"
//     case InstrSize.Word => "%r8w"
//     case InstrSize.DWord => "%r8d"
//     case InstrSize.QWord => "%r8"
//   }
// }

// case class R9(sizeType: InstrSize.Size = InstrSize.QWord) extends Reg {
//   override def toString: String = sizeType match {
//     case InstrSize.Byte => "%r9b"
//     case InstrSize.Word => "%r9w"
//     case InstrSize.DWord => "%r9d"
//     case InstrSize.QWord => "%r9"
//   }
// }

// case class R10(sizeType: InstrSize.Size = InstrSize.QWord) extends Reg {
//   override def toString: String = sizeType match {
//     case InstrSize.Byte => "%r10b"
//     case InstrSize.Word => "%r10w"
//     case InstrSize.DWord => "%r10d"
//     case InstrSize.QWord => "%r10"
//   }
// }

// case class R11(sizeType: InstrSize.Size = InstrSize.QWord) extends Reg {
//   override def toString: String = sizeType match {
//     case InstrSize.Byte => "%r11b"
//     case InstrSize.Word => "%r11w"
//     case InstrSize.DWord => "%r11d"
//     case InstrSize.QWord => "%r11"
//   }
// }

// case class R12(sizeType: InstrSize.Size = InstrSize.QWord) extends Reg {
//   override def toString: String = sizeType match {
//     case InstrSize.Byte => "%r12b"
//     case InstrSize.Word => "%r12w"
//     case InstrSize.DWord => "%r12d"
//     case InstrSize.QWord => "%r12"
//   }
// }

// case class R13(sizeType: InstrSize.Size = InstrSize.QWord) extends Reg {
//   override def toString: String = sizeType match {
//     case InstrSize.Byte => "%r13b"
//     case InstrSize.Word => "%r13w"
//     case InstrSize.DWord => "%r13d"
//     case InstrSize.QWord => "%r13"
//   }
// }

// case class R14(sizeType: InstrSize.Size = InstrSize.QWord) extends Reg {
//   override def toString: String = sizeType match {
//     case InstrSize.Byte => "%r14b"
//     case InstrSize.Word => "%r14w"
//     case InstrSize.DWord => "%r14d"
//     case InstrSize.QWord => "%r14"
//   }
// }

// case class R15(sizeType: InstrSize.Size = InstrSize.QWord) extends Reg {
//   override def toString: String = sizeType match {
//     case InstrSize.Byte => "%r15b"
//     case InstrSize.Word => "%r15w"
//     case InstrSize.DWord => "%r15d"
//     case InstrSize.QWord => "%r15"
//   }
// }

}