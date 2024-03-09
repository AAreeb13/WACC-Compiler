package wacc

import IR._
import scala.language.implicitConversions

/*
 * This class encapsulates key information 
 * related to how registers, sizes and operations
 * are represented within architectures
 */



abstract class TargetConfig {
    val ReturnReg: Register
    val StackPointer: Register
    val BasePointer: Register
    val ArrayPointer: Register
    val ArrayPointerPointer: Register
    val IndexPointer: Register
    val ScratchRegs: List[Register]
    val InstructionPointer: Register
    val DivRegister: Register
    val ModRegister: Register

    val ParamRegs: List[Register]
    val CallerSaved: List[Register]
    val CalleeSaved: List[Register]

    val allRegs = List(R0, R1, R2, R3, R4, R5, R6, R7, R8, R9, R10, R11, R12, R13, R14, R15)

    implicit def opStr(op: Operand)(implicit size: Size = QWord): String
    implicit def sizeStr(size: Size): String
    implicit def flagStr(flag: Condition): String

    def assemble(ir: List[Line]): String
}

