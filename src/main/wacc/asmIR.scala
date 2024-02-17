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
}

object InstrSize extends Enumeration {
    type Size = Value
    val Byte = Value("b")
    val Word = Value("")
    val DWord = Value("d")
    val QWord = Value("q")
}

sealed trait ASMItem {
    def format: String = ???
}

case class Label(ident: String) extends ASMItem {
    override def toString() = s"$ident:"
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

case class StringDecl(strVal: String, labelName: String) extends Section {
    override def toString() =
        s"\t.int ${strVal.length}\n" + 
        labelName + ": \n" + 
        s"\t.asciz \"${strVal}\""
}

sealed trait Sign extends ASMItem
case object Positive extends Sign {
    override def toString() = "+"
}
case object Negative extends Sign {
    override def toString() = "-"
}

sealed trait Operand extends ASMItem

case class ImmVal(value: Int) extends Operand {
    override def toString() = s"$$$value"
}

case class Mem(reg: Operand, offset: Option[(Sign, Option[Int], Operand)]) extends Operand {
    override def toString() = {
        val sb = new StringBuilder
        sb ++= "["
        sb ++= reg.toString()
        offset match {
            case None =>
            case Some((sign, mul, op)) =>
                sb ++= sign.toString()
                if (mul.isDefined) {
                    sb ++= s"${mul.get}*"
                }
                sb ++= op.toString()
        }
        sb ++= "]"
        sb.toString()
    }
}

sealed trait Instr extends ASMItem

case object Ret extends Instr {
    override def toString() = "ret"
}
case class Mov(dst: Operand, src: Operand, size: InstrSize.Size) extends Instr {
    override def toString() = s"mov${size} ${src} ${dst}"
}
case class Movs(dst: Operand, src: Operand, sizeFrom: InstrSize.Size, sizeTo: InstrSize.Size) extends Instr {
    override def toString() = s"mov${sizeFrom}${sizeTo} ${src} ${dst}"
}

case class Pop(reg: Operand, size: InstrSize.Size) extends Instr {
    override def toString() = s"pop${size} ${reg}"
}
case class Push(reg: Operand, size: InstrSize.Size) extends Instr {
    override def toString() = s"push${size} ${reg}"
}

case class Set(reg: Operand) {
    override def toString() = s"set ${reg}"
}

case class CMov(dst: Operand, src: Operand, flag: ComparisonType.Flag) extends Instr {
    override def toString() = s"cmov${flag} ${src} ${dst}"
}
case class J(label: Label, flag: ComparisonType.Flag) extends Instr {
    override def toString() = s"j${flag} ${label}"
}

case class Call(func: Label) extends Instr {
    override def toString() = s"call ${func}"
}
case class Jmp(label: Label) extends Instr {
    override def toString() = s"jmp ${label}"
}

case class Cmp(dst: Operand, src: Operand, size: InstrSize.Size) extends Instr {
    override def toString() = s"cmp${size} ${src} ${dst}"
}
case class Lea(dst: Operand, src: Operand, size: InstrSize.Size) extends Instr {
    override def toString() = s"lea${size} ${src} ${dst}"
}

case class Add(dst: Operand, src: Operand, size: InstrSize.Size) extends Instr {
    override def toString() = s"add${size} ${src} ${dst}"
}
case class Sub(dst: Operand, src: Operand, size: InstrSize.Size) extends Instr {
    override def toString() = s"sub${size} ${src} ${dst}"
}
case class And(dst: Operand, src: Operand, size: InstrSize.Size) extends Instr {
    override def toString() = s"and${size} ${src} ${dst}"
}
case class IDiv(op: Operand, size: InstrSize.Size) extends Instr {
    override def toString() = "cltd\n" + s"idiv${size} ${op}"
}

sealed trait Reg extends Operand

case object Rax extends Reg {
  override def toString: String = "rax"
}
case object Rbx extends Reg {
  override def toString: String = "rbx"
}
case object Rcx extends Reg {
  override def toString: String = "rcx"
}
case object Rdx extends Reg {
  override def toString: String = "rdx"
}
case object Rsi extends Reg {
  override def toString: String = "rsi"
}
case object Rdi extends Reg {
  override def toString: String = "rdi"
}
case object Rbp extends Reg {
  override def toString: String = "rbp"
}
case object Rsp extends Reg {
  override def toString: String = "rsp"
}

case object R8 extends Reg {
  override def toString: String = "r8"
}
case object R9 extends Reg {
  override def toString: String = "r9"
}
case object R10 extends Reg {
  override def toString: String = "r10"
}
case object R11 extends Reg {
  override def toString: String = "r11"
}
case object R12 extends Reg {
  override def toString: String = "r12"
}
case object R13 extends Reg {
  override def toString: String = "r13"
}
case object R14 extends Reg {
  override def toString: String = "r14"
}
case object R15 extends Reg {
  override def toString: String = "r15"
}
}