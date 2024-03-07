package wacc

import ast._

object TAC {
    sealed trait TAC

    case class BinaryOpTAC(op: BinOp, t1: OperandTAC, t2: OperandTAC, res: LocationTAC) extends TAC {
        override def toString(): String = s"$res = $t1 $op $t2"
    }

    case class UnaryOpTAC(op: UnOp, t1: OperandTAC, res: LocationTAC) extends TAC {
        override def toString(): String = s"$res = $op $t1"
    }

    case class AssignTAC(t1: OperandTAC, res: LocationTAC) extends TAC {
        override def toString(): String = s"$res = $t1"
    }

    case class IfTAC(cond: OperandTAC, label: String) extends TAC {
        override def toString(): String = s"if $cond goto $label"
    }

    case class AllocateStackTAC(size: Int) extends TAC {
        override def toString(): String = s"alloc_stack $size"
    }


    sealed trait OperandTAC

    sealed trait LocationTAC extends OperandTAC

    case class RegisterTAC(id: Int) extends OperandTAC



    sealed trait LiteralTAC extends OperandTAC


    
    
}