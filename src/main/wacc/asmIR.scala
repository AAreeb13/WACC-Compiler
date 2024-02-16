package wacc

object asmIR {

case class Label(ident: String)

object StatusFlag extends Enumeration {
    type Flag = Value
    val Zero, Overflow, Sign, Carry = Value
}

object ComparisonType extends Enumeration {
    type Flag = Value
    val GreaterThan, Zero, Equal, NotEqual, LessThan, Overflow = Value
}

object InstrSize extends Enumeration {
    type Size = Value
    val Byte, Word, DWord, QWord = Value
}

sealed trait ASMItem {
    def format: String = ???
}

sealed trait Section extends ASMItem

case object Readonly extends Section
case object Text extends Section
case object Global extends Section

case class StringDecl(strVal: String, labelName: String) extends Section

sealed trait Sign extends ASMItem
case object Positive extends Sign
case object Negative extends Sign

sealed trait Operand extends ASMItem
case class Mem(reg: Operand, offset: Option[(Sign, Option[Int], Operand)]) extends Operand

sealed trait Instr extends ASMItem

case object Ret extends Instr
case class Mov(dst: Operand, src: Operand, size: InstrSize.Size) extends Instr
case class Movs(dst: Operand, src: Operand, sizeFrom: InstrSize.Size, sizeTo: InstrSize.Size) extends Instr

case class Pop(reg: Operand, size: InstrSize.Size) extends Instr
case class Push(reg: Operand, size: InstrSize.Size) extends Instr
case class Set(reg: Operand)

case class CMov(dst: Operand, src: Operand, flag: ComparisonType.Flag) extends Instr
case class J(label: Label, flag: ComparisonType.Flag) extends Instr

case class Call(func: Label) extends Instr
case class Jmp(func: Label) extends Instr

case class Cmp(dst: Operand, src: Operand, size: InstrSize.Size) extends Instr
case class Lea(dst: Operand, src: Operand, size: InstrSize.Size) extends Instr

case class Add(dst: Operand, src: Operand, size: InstrSize.Size) extends Instr
case class Sub(dst: Operand, src: Operand, size: InstrSize.Size) extends Instr
case class And(dst: Operand, src: Operand, size: InstrSize.Size) extends Instr
case class Or(dst: Operand, src: Operand, size: InstrSize.Size) extends Instr
case class Div(dst: Operand, src: Operand, size: InstrSize.Size) extends Instr

sealed trait Reg extends Operand

case object Rax extends Reg
case object Rbx extends Reg
case object Rcx extends Reg
case object Rdx extends Reg
case object Rsi extends Reg
case object Rdi extends Reg
case object Rbp extends Reg
case object Rsp extends Reg

case object R8 extends Reg
case object R9 extends Reg
case object R10 extends Reg
case object R11 extends Reg
case object R12 extends Reg
case object R13 extends Reg
case object R14 extends Reg
case object R15 extends Reg

case class ImmVal(value: Int) extends Operand

}