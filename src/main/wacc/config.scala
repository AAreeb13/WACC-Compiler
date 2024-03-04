package wacc

import IR._

abstract class TargetConfig {
    val ReturnReg: Register
    val StackPointer: Register
    val BasePointer: Register
    val ScratchReg: Register

    val ParamRegs: List[Register]
    val CallerSaved: List[Register]
    val CalleeSaved: List[Register]

    val allRegs = List(R0, R1, R2, R3, R4, R5, R6, R7, R8, R9, R10, R11, R12, R13, R14, R15)
}

case object X86Config extends TargetConfig {
    val Rax = R0
    val Rbx = R1
    val Rcx = R2
    val Rdx = R3
    val Rsi = R4
    val Rdi = R5
    val Rsp = R6
    val Rbp = R7

    override val ReturnReg: Register = Rax
    override val ScratchReg: Register = Rax
    override val StackPointer: Register = Rsp
    override val BasePointer: Register = Rbp

    override val ParamRegs: List[Register] = List(Rdi, Rsi, Rdx, Rcx, R8, R9)
    override val CallerSaved: List[Register] = List(Rax, R10, R11) ++ ParamRegs
    override val CalleeSaved: List[Register] = allRegs.diff(CallerSaved)
}

