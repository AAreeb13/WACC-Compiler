package wacc

sealed trait SemNode

sealed trait SemType extends SemNode {
}

sealed trait SemBaseType

case object SemInt extends SemBaseType
case object SemBool extends SemBaseType
case object SemChar extends SemBaseType
case object SemString extends SemBaseType

case object SemNull extends SemBaseType
// null keyword for pairvla

case object SemAny extends SemType
// any type

case object SemUnknown extends SemType
// nested pair extraction type

case object SemNone extends SemType
// error type

case class SemPairType(t1: SemType, t2: SemType) extends SemType
case class SemArrayType(t: SemType) extends SemType
