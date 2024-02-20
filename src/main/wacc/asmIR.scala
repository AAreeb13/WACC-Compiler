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

case class ImmVal(value: BigInt) extends Operand {
    override def toString() = s"$$$value"
}

case class Mem(reg: Reg, offset: Option[(Operand, Option[Int])]) extends Operand {
    override def toString: String = offset match {
        case None                               => s"($reg)"
        case Some((ImmVal(value), None))        => s"$value($reg)"
        case Some((offsetReg: Reg, None))       => s"($reg, $offsetReg)"
        case Some((offsetReg: Reg, Some(mul)))  => s"($reg, $offsetReg, $mul)"
        case _ => "error"
    }
}

object Mem {
    def apply(reg: Reg): Mem = Mem(reg, None)
    def apply(reg: Reg, offsetOp: Operand): Mem = Mem(reg, Some((offsetOp, None)))
    def apply(reg: Reg, offsetOp: Operand, multiplier: Int): Mem = Mem(reg, Some((offsetOp, Some(multiplier))))
}


sealed trait Instr extends ASMItem

case object Ret extends Instr {
    override def toString() = "ret"
}
case class Mov(src: Operand, dst: Operand, size: InstrSize.Size = InstrSize.QWord) extends Instr {
    override def toString() = s"mov${size} ${src}, ${dst}"
}
case class Movs(src: Operand, dst: Operand, sizeFrom: InstrSize.Size, sizeTo: InstrSize.Size) extends Instr {
    override def toString() = s"mov${sizeFrom}${sizeTo} ${src}, ${dst}"
}

case class Pop(reg: Operand, size: InstrSize.Size = InstrSize.QWord) extends Instr {
    override def toString() = s"pop${size} ${reg}"
}
case class Push(reg: Operand, size: InstrSize.Size = InstrSize.QWord) extends Instr {
    override def toString() = s"push${size} ${reg}"
}

case class Set(reg: Operand) {
    override def toString() = s"set ${reg}"
}

case class CMov(src: Operand, dst: Operand, flag: ComparisonType.Flag) extends Instr {
    override def toString() = s"cmov${flag} ${src}, ${dst}"
}
case class J(label: Label, flag: ComparisonType.Flag) extends Instr {
    override def toString() = s"j${flag} ${label}"
}

case class Call(callable: Callable) extends Instr {
    override def toString() = s"call ${callable}"
}
case class Jmp(label: Label) extends Instr {
    override def toString() = s"jmp ${label}"
}

case class Cmp(src: Operand, dst: Operand, size: InstrSize.Size = InstrSize.QWord) extends Instr {
    override def toString() = s"cmp${size} ${src}, ${dst}"
}
case class Lea(src: Operand, dst: Operand, size: InstrSize.Size = InstrSize.QWord) extends Instr {
    override def toString() = s"lea${size} ${src}, ${dst}"
}

case class Add(src: Operand, dst: Operand, size: InstrSize.Size = InstrSize.QWord) extends Instr {
    override def toString() = s"add${size} ${src}, ${dst}"
}
case class Sub(src: Operand, dst: Operand, size: InstrSize.Size = InstrSize.QWord) extends Instr {
    override def toString() = s"sub${size} ${src}, ${dst}"
}
case class And(src: Operand, dst: Operand, size: InstrSize.Size = InstrSize.QWord) extends Instr {
    override def toString() = s"and${size} ${src}, ${dst}"
}
case class IDiv(op: Operand, size: InstrSize.Size = InstrSize.QWord) extends Instr {
    override def toString() = "cltd\n" + s"idiv${size} ${op}"
}

sealed trait Reg extends Operand

case object Rax extends Reg {
  override def toString: String = "%rax"
}
case object Rbx extends Reg {
  override def toString: String = "%rbx"
}
case object Rcx extends Reg {
  override def toString: String = "%rcx"
}
case object Rdx extends Reg {
  override def toString: String = "%rdx"
}
case object Rsi extends Reg {
  override def toString: String = "%rsi"
}
case object Rdi extends Reg {
  override def toString: String = "%rdi"
}
case object Rbp extends Reg {
  override def toString: String = "%rbp"
}
case object Rsp extends Reg {
  override def toString: String = "%rsp"
}

case object R8 extends Reg {
  override def toString: String = "%r8"
}
case object R9 extends Reg {
  override def toString: String = "%r9"
}
case object R10 extends Reg {
  override def toString: String = "%r10"
}
case object R11 extends Reg {
  override def toString: String = "%r11"
}
case object R12 extends Reg {
  override def toString: String = "%r12"
}
case object R13 extends Reg {
  override def toString: String = "%r13"
}
case object R14 extends Reg {
  override def toString: String = "%r14"
}
case object R15 extends Reg {
  override def toString: String = "%r15"
}
}